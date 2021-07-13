package zio.macros

import scala.reflect.macros.whitebox

private[macros] object LayerMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def abort(msg: String) =
      c.abort(c.enclosingPosition, msg)

    val outTpe = c.macroApplication match {
      case Apply(Select(Apply(Select(New(AppliedTypeTree(_, out :: Nil)), _), _), _), _) =>
        out
      case _ =>
        abort("Missing output type")
    }

    def makeLayer(tpname: TypeName) =
      q"""
          def layer(implicit gen: _root_.zio.macros.ZLayerFromConstructor[$tpname, $outTpe]): _root_.zio.ZLayer[gen.In, Nothing, _root_.zio.Has[$outTpe]] = 
            gen.layer
        """

    def result =
      annottees.map(_.tree) match {
        case (classDef @ q"$mods class $tpname[..$_] $_(..$_) extends { ..$_ } with ..$_ { $_ => ..$_ }")
            :: Nil =>
          q"""
         $classDef
         $mods object ${TermName(tpname.toString())} {
           ${makeLayer(tpname)}
         }
         """
        case (classDef @ q"$_ class $tpname[..$_] $_(..$_) extends { ..$_ } with ..$_ { $_ => ..$_ }")
            :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
            :: Nil =>
          q"""
         $classDef
         $objMods object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ${makeLayer(tpname)}
           ..$objDefs
         }
         """
        case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a class")
      }

    c.Expr[Any](result)
  }
}
