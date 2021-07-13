package zio.macros

import scala.annotation.{compileTimeOnly, StaticAnnotation}

/**
 * Adds layer method to companion object.
 *
 * Example:
 *
 * {{{
 *   trait Foo
 *
 *   @layer[Foo]
 *   class DefaultFoo(int: Int, string: String) extends Foo
 * }}}
 *
 * will expand to
 *
 * {{{
 *   trait Foo
 *
 *   class DefaultFoo(int: Int, string: String) extends Foo
 *
 *   object DefaultFoo {
 *     def layer(implicit lfc: LayerFromConstructor[DefaultFoo, Foo]): ZLayer[lfc.In, Nothing, Has[Foo]] =
 *       lfc.layer
 *   }
 * }}}
 *
 * @tparam A layer output
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class layer[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro LayerMacro.impl
}
