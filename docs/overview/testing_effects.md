---
id: overview_testing_effects
title:  "Testing Effects"
---

There are many approaches to testing functional effects, including using free monads, using tagless-final, and using environmental effects. Although all of these approaches are compatible with ZIO, the simplest and most ergonomic is _environmental effects_.

This section introduces environmental effects and shows you how to write testable functional code using them.

```scala mdoc
import zio._
import zio.console._
```

## Environments

The ZIO data type has an `R` type parameter, which is used to describe the type of _environment_ required by the effect. 

ZIO effects can access the environment using `ZIO.environment`, which provides direct access to the environment, as a value of type `R`.

```scala mdoc:silent
for {
  env <- ZIO.environment[Int]
  _   <- putStrLn(s"The value of the environment is: $env")
} yield env
```

The environment does not have to be a primitive value like an integer. It can be much more complex, like a `trait` or `case class`.

When the environment is a type with fields, then the `ZIO.access` method can be used to access a given part of the environment in a single method call:

```scala mdoc:silent:nest
final case class Config(server: String, port: Int)

val configString: URIO[Config, String] = 
  for {
    server <- ZIO.access[Config](_.server)
    port   <- ZIO.access[Config](_.port)
  } yield s"Server: $server, port: $port"
```

Even effects themselves can be stored in the environment! In this case, to access and execute an effect, the `ZIO.accessM` method can be used.

```scala mdoc:silent
trait DatabaseOps {
  def getTableNames: Task[List[String]]
  def getColumnNames(table: String): Task[List[String]]
}

val tablesAndColumns: ZIO[DatabaseOps, Throwable, (List[String], List[String])] = 
  for {
    tables  <- ZIO.accessM[DatabaseOps](_.getTableNames)
    columns <- ZIO.accessM[DatabaseOps](_.getColumnNames("user_table"))
  } yield (tables, columns)
```

When an effect is accessed from the environment, as in the preceding example, the effect is called an _environmental effect_.

Later, we'll see how environmental effects provide an easy way to test ZIO applications.

### Providing Environments

Effects that require an environment cannot be run without first _providing_ their environment to them.

The simplest way to provide an effect the environment that it requires is to use the `ZIO#provide` method:

```scala mdoc:silent
val square: URIO[Int, Int] = 
  for {
    env <- ZIO.environment[Int]
  } yield env * env

val result: UIO[Int] = square.provide(42)
```

Once you provide an effect with the environment it requires, then you get back an effect whose environment type is `Any`, indicating its requirements have been fully satisfied.

The combination of `ZIO.accessM` and `ZIO#provide` are all that is necessary to fully use environmental effects for easy testability.

## Environmental Effects

The fundamental idea behind environmental effects is to _program to an interface, not an implementation_. In the case of functional Scala, interfaces do not contain any methods that perform side-effects, although they may contain methods that return _functional effects_.

Rather than passing interfaces throughout our code base manually, injecting them using dependency injection, or threading them using incoherent implicits, we use _ZIO Environment_ to do the heavy lifting, which results in elegant, inferrable, and painless code.

In this section, we'll explore how to use environmental effects by developing a testable database service.

### Define the Service

We will define the database service with the help of a module, which is an interface that contains only a single field, which provides access to the service.

```scala mdoc
trait UserID
trait UserProfile
val userId = new UserID { }
```

```scala mdoc:silent
object Database {
  trait Service {
    def lookup(id: UserID): Task[UserProfile]
    def update(id: UserID, profile: UserProfile): Task[Unit]
  }
}
trait Database {
  def database: Database.Service
}
```

In this example,  `Database` is the _module_, which contains the `Database.Service` _service_. The _service_ is just an ordinary interface, placed inside the companion object of the module, which contains functions provide the _capabilities_ of the service.

### Provide Helpers

In order to make it easier to access the database service as an environmental effect, we will define helper functions that use `ZIO.accessM`.

```scala mdoc:silent
object db {
  def lookup(id: UserID): RIO[Database, UserProfile] =
    ZIO.accessM(_.database.lookup(id))

  def update(id: UserID, profile: UserProfile): RIO[Database, Unit] =
    ZIO.accessM(_.database.update(id, profile))
}
```

While these helpers are not required, because we can access the database module directly through `ZIO.accessM`, the helpers are easy to write and make use-site code simpler.

### Use the Service

Now that we have defined a module and helper functions, we are now ready to build an example that uses the database service:

```scala mdoc:silent
val lookedupProfile: RIO[Database, UserProfile] = 
  for {
    profile <- db.lookup(userId)
  } yield profile
```

The effect in this example interacts with the database solely through the environment, which in this case, is a module that provides access to the database service.

To actually run such an effect, we need to provide an implementation of the database module.

### Implement Live Service

Now we will implement a live database module, which will actually interact with our production database:

```scala mdoc:silent
trait DatabaseLive extends Database {
  def database: Database.Service = 
    new Database.Service {
      def lookup(id: UserID): Task[UserProfile] = ???
      def update(id: UserID, profile: UserProfile): Task[Unit] = ???
    }
}
object DatabaseLive extends DatabaseLive
```

In the preceding snippet, the implementation of the two database methods is not provided, because that would require details beyond the scope of this tutorial.

### Run the Database Effect

We now have a database module, helpers to interact with the database module, and a live implementation of the database module. 

We can now provide the live database module to our application, using `ZIO.provide`:

```scala mdoc:silent
def main: RIO[Database, Unit] = ???

def main2: Task[Unit] = 
  main.provide(DatabaseLive)
```

The resulting effect has no requirements, so it can now be executed with a ZIO runtime.

### Implement Test Service

To test code that interacts with the database, we don't want to interact with a real database, because our tests would be slow and brittle, and fail randomly even when our application logic is correct.

Although you can use mocking libraries to create test modules, in this section, we will simply create a test module directly, to show that no magic is involved:

```scala mdoc:silent
class TestService extends Database.Service {
  private var map: Map[UserID, UserProfile] = Map()

  def setTestData(map0: Map[UserID, UserProfile]): Task[Unit] = 
    Task { map = map0 }

  def getTestData: Task[Map[UserID, UserProfile]] = 
    Task(map)

  def lookup(id: UserID): Task[UserProfile] = 
    Task(map(id))

  def update(id: UserID, profile: UserProfile): Task[Unit] = 
    Task.effect { map = map + (id -> profile) }
}
trait TestDatabase extends Database {
  val database: TestService = new TestService
}
object TestDatabase extends TestDatabase
```

Because this module will only be used in tests, it simulates interaction with a database by extracting and updating data in a hard-coded map. To make this module fiber-safe, you could instead use a `Ref` and not a `var` to hold the map.

### Test Database Code

To test code that requires the database, we need only provide it with our test database module:

```scala mdoc:silent
def code: RIO[Database, Unit] = ???

def code2: Task[Unit] = 
  code.provide(TestDatabase)
```

Our application code can work with either our production database module, or the test database module.

## Next Steps

If you are comfortable with testing effects, then the next step is to learn about [running effects](running_effects.md).
