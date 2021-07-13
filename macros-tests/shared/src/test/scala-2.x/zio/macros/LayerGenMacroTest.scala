package zio.macros

import zio.test.Assertion._
import zio.test._

object LayerGenMacroTest extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("LayerFromConstructorSpec")(
      suite("ZLayer.fromConstructor syntax")(
        testM("generates layer from constructor") {
          assertM(typeCheck {
            """
              import zio._
              
              trait Bar
              
              final class Foo(int: Int, string: String) extends Bar
              
              trait Baz
              
              final class X extends Baz
              
              object Test {
                val bar: zio.ZLayer[Has[Int] with Has[String], Nothing, Has[Bar]] = 
                 ZLayer.fromConstructor[Foo, Bar]
                  
                val baz: ZLayer[Any, Nothing, Has[Baz]] = 
                 ZLayer.fromConstructor[X, Baz]
              }
            """
          })(isRight(anything))
        },
        testM("fails when primary constructor has parameters of the same type") {
          assertM(typeCheck {
            """
              import zio._
              
              trait Bar
              
              final class Foo(int: Int, another: Int) extends Bar
              
              object Test {
                ZLayer.fromConstructor[Foo, Bar]
              }
            """
          })(isLeft(anything))
        }
      )
    )
}
