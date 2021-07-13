package zio.test.magnolia

import zio.test._
import zio.test.magnolia.diff._

object DeriveDiffSpec extends DefaultRunnableSpec {
  final case class Pet(name: String, hasBone: Boolean, favoriteFoods: List[String])
  final case class Person(name: String, age: Int, pet: Pet)

  def spec = suite("DeriveDiffSpec")(
    suite("derivation")(
      test("case classes can be derived") {
        val l1 = List("Alpha", "This is a wonderful \"way\" to dance and party", "Potato")
        val l2 = List("Alpha", "This is a wonderful way to live and die", "Potato", "Bruce Lee", "Potato", "Ziverge")
        val p1 = Person("Boboo", 300, Pet("The Beautiful Crumb", false, l1))
        val p2 = Person("Bibi", 300, Pet("The Beautiful Destroyer", false, l2))
        assertTrue(p1 == p2)
      } @@ TestAspect.failing
    )
  )
}
