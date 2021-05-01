package zio.examples
import zio._
import zio.console.Console

object LayerDefinitionExample extends App {
  trait Foo {
    def bar: UIO[Unit]
  }

  object Foo {
    val live: URLayer[Console with Has[String] with Has[Int], Has[Foo]] =
      Ref.make(true).toLayer >>> (FooLive.apply _).toLayer[Foo]

    case class FooLive(console: Console.Service, string: String, int: Int, ref: Ref[Boolean]) extends Foo {
      override def bar: UIO[Unit] =
        ref.get.flatMap { bool =>
          console.putStrLn(s"$string and $int and Ref($bool)")
        }
    }
  }


  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val liveLayer: ULayer[Has[Foo]] =
      (Console.live ++ ZLayer.succeed("Hello") ++ ZLayer.succeed(3)) >>> Foo.live

    ZIO
      .accessM[Has[Foo]](_.get.bar)
      .provideLayer(liveLayer)
      .exitCode
  }


  // Inferrable remainder examples
  val layer: URLayer[Has[Int] with Has[String], Has[Boolean]] = ZLayer.succeed(true)
  val int: ULayer[Has[Int]] = ZLayer.succeed(1)
  val both: ULayer[Has[Int] with Has[String]] = ZLayer.succeed(1) ++ ZLayer.succeed("hi")

  val c1: ZLayer[Has[String], Nothing, Has[Boolean]] = int >>> layer
  val c2: ZLayer[Any, Nothing, Has[Boolean]] = both >>> layer
}
