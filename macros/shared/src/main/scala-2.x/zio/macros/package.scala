package zio

import scala.annotation.unused

package object macros {

  implicit final class ZLayerFromConstructorSyntax(@unused private val self: ZLayer.type) extends AnyVal {

    /**
     * Generates a [[ZLayer]] of type [[Out]] from primary constructor of type [[From]].
     *
     * All parameters of the constructor become the layer input.
     *
     * @param gen layer generator provided by a macro
     * @tparam From type to instantiate
     * @tparam Out resulting layer output
     */
    def fromConstructor[From <: Out, Out](implicit
      gen: ZLayerFromConstructor[From, Out]
    ): ZLayer[gen.In, Nothing, Has[Out]] =
      gen.layer
  }
}
