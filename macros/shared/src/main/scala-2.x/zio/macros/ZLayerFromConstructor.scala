package zio.macros

import zio.{Has, ZLayer}

/**
 * Represent the ability to construct a [[ZLayer]] with the resulting type [[Out]]
 * from the primary constructor of type [[From]]
 */
trait ZLayerFromConstructor[From <: Out, Out] {

  /**
   * The input type of resulting layer.
   */
  type In

  def layer: ZLayer[In, Nothing, Has[Out]]
}

object ZLayerFromConstructor {
  type Aux[From <: Out, Out, In0] = ZLayerFromConstructor[From, Out] {
    type In = In0
  }

  def apply[From <: Out, Out](implicit lfc: ZLayerFromConstructor[From, Out]): Aux[From, Out, lfc.In] = lfc

  implicit def materialize[From <: Out, Out, In]: ZLayerFromConstructor.Aux[From, Out, In] =
    macro ZLayerFromConstructorMacro.materialize[From, Out, In]
}
