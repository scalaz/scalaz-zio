package zio.macros

import zio.test.Assertion._
import zio.test._

object LayerSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] = suite("LayerSpec")(
    suite("Layer macro")(
      testM("generates a companion object with a layer method") {
        assertM(typeCheck {
          """
            import zio._
            
            trait Bar
            
            @layer[Bar]
            class Foo(int: Int, string: String) extends Bar
            
            object Test {
              val bar: ZLayer[Has[Int] with Has[String], Nothing, Has[Bar]] = Foo.layer
            }
          """
        })(isRight(anything))
      },
      testM("generates a layer method and preserves the contents of existing companion") {
        assertM(typeCheck {
          """
            import zio._
            
            trait Bar
            
            @layer[Bar]
            class Foo(int: Int, string: String) extends Bar
            
            object Foo {
               def unit: Unit = ()
            }
            
            object Test {
              val layer: ZLayer[Has[Int] with Has[String], Nothing, Has[Bar]] = Foo.layer
              val unit: Unit = Foo.unit
            }
          """
        })(isRight(anything))
      }
    )
  )
}
