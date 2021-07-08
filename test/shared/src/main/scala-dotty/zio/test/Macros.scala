/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.test

import zio.{UIO, ZIO}
import scala.quoted._

object SmartAssertMacros {
  def smartAssertSingle(expr: Expr[Boolean])(using ctx: Quotes): Expr[Assert] =
    new SmartAssertMacros(ctx).smartAssertSingle_impl(expr)

  def smartAssert(exprs: Expr[Seq[Boolean]])(using ctx: Quotes): Expr[Assert] =
    new SmartAssertMacros(ctx).smartAssert_impl(exprs)
}


class SmartAssertMacros(ctx: Quotes)  {
  given Quotes = ctx
  import quotes.reflect._

  extension (typeRepr: TypeRepr) {
    def typeTree: TypeTree =
      typeRepr.widen.asType match  {
        case '[tp] => TypeTree.of[tp]
      }
  }


  object MethodCall {
    def unapply(tree: Term): Option[(Term, String, List[TypeRepr], Option[List[Term]])] =
      tree match {
        case Select(lhs, name) => Some((lhs, name, List.empty, None))
        case TypeApply(Select(lhs, name), tpes) => Some((lhs, name, tpes.map(_.tpe), None))
        case Apply(Select(lhs, name), args) => Some((lhs, name, List.empty, Some(args)))
        case Apply(TypeApply(Select(lhs, name), tpes), args) => Some((lhs, name, tpes.map(_.tpe), Some(args)))
        case _ => None
      }
  }

  case class PositionContext(start: Int) 

  object PositionContext {
    def apply(term: Term) = new PositionContext(term.pos.start)
  }

  extension (term: Term)(using ctx: PositionContext) {
    def span: Expr[(Int, Int)] = {
      Expr(term.pos.start - ctx.start, term.pos.end - ctx.start)
    }
  }

  def transform[A](expr: Expr[A])(using Type[A], PositionContext) : Expr[Arrow[Any, A]] =
    expr match {
      case Unseal(Inlined(_, _, expr)) => transform(expr.asExprOf[A])

      case Unseal(Apply(Select(lhs, op @ (">" | ">=" | "<" | "<=")), List(rhs))) =>
        val span = rhs.span
        lhs.tpe.widen.asType match {
          case '[l] => 
            Expr.summon[Ordering[l]] match { 
              case Some(ord) =>
                op match {
                    case ">" =>
                      '{${transform(lhs.asExprOf[l])} >>> Assertions.greaterThan(${rhs.asExprOf[l]})($ord).span($span)}.asExprOf[Arrow[Any, A]]
                    case ">=" =>
                      '{${transform(lhs.asExprOf[l])} >>> Assertions.greaterThanOrEqualTo(${rhs.asExprOf[l]})($ord).span($span)}.asExprOf[Arrow[Any, A]]
                    case "<" =>
                      '{${transform(lhs.asExprOf[l])} >>> Assertions.lessThan(${rhs.asExprOf[l]})($ord).span($span)}.asExprOf[Arrow[Any, A]]
                    case "<=" =>
                      '{${transform(lhs.asExprOf[l])} >>> Assertions.lessThanOrEqualTo(${rhs.asExprOf[l]})($ord).span($span)}.asExprOf[Arrow[Any, A]]
                }
              case _ => throw new Error("NO")
            }
        }

      case Unseal(MethodCall(lhs, "==", tpes, Some(List(rhs)))) =>
        val span = rhs.span
        lhs.tpe.widen.asType match {
          case '[l] => 
            '{${transform(lhs.asExprOf[l])} >>> Assertions.equalTo(${rhs.asExprOf[l]}).span($span)}.asExprOf[Arrow[Any, A]]
        }

      // case method @AST.Method(lhs, lhsTpe, rhsTpe, name, tpeArgs, args, span) =>
      case Unseal(method @ MethodCall(lhs, name, tpeArgs, args)) =>
        def body(param: Term) =
          (tpeArgs, args) match {
            case (Nil, None) => Select.unique(param, name)
            case (tpeArgs, Some(args)) => Select.overloaded(param, name, tpeArgs, args)
            case (tpeArgs, None) => Select.overloaded(param, name, tpeArgs, List.empty)
          }

        val tpe = lhs.tpe.widen

        if(tpe.typeSymbol.isPackageDef)
          '{Arrow.succeed($expr).span(${method.span})}
        else
          tpe.asType match {
            case '[l] =>
              val selectBody = '{
                (from: l) => ${ body('{from}.asTerm).asExprOf[A] }
              }
              val lhsExpr = transform(lhs.asExprOf[l]).asExprOf[Arrow[Any, l]]
              val assertExpr = '{Arrow.fromFunction[l, A](${selectBody})}
              val pos = summon[PositionContext]
              val span = Expr((lhs.pos.end - pos.start, method.pos.end - pos.start))
              '{$lhsExpr >>> $assertExpr.span($span)}
          }


      case Unseal(tree) =>
        val span = tree.span
       '{Arrow.succeed($expr).span($span)}
    }

  def smartAssertSingle_impl(value: Expr[Boolean]): Expr[Assert] = {
    val (path, line) = Macros.location(ctx)
    val code = Macros.showExpr(value)
    val srcLocation = s"$path:$line"

    implicit val ptx = PositionContext(value.asTerm)

    val ast = transform(value)

    val arrow = ast.asExprOf[Arrow[Any, Boolean]]
    '{Assert($arrow.withCode(${Expr(code)}).withLocation(${Expr(srcLocation)}))}
  }

  def smartAssert_impl(values: Expr[Seq[Boolean]]): Expr[Assert] = {
    import quotes.reflect._

    values match {
        case Varargs(head +: tail) =>
          tail.foldLeft(smartAssertSingle_impl(head)) { (acc, expr) =>
            '{$acc && ${smartAssertSingle_impl(expr)}}
        }

        case other =>
          throw new Error(s"Improper Varargs: ${other}")
    }
  }

  object Unseal {
    def unapply(expr: Expr[_]): Option[Term] = Some(expr.asTerm)
  }
}

object Macros {
  def location(ctx: Quotes): (String, Int) = {
    import ctx.reflect._
    val path = Position.ofMacroExpansion.sourceFile.jpath.toString
    val line = Position.ofMacroExpansion.startLine + 1
    (path, line)
  }

  def assertM_impl[R: Type, E: Type, A: Type](effect: Expr[ZIO[R, E, A]])(assertion: Expr[AssertionM[A]])
                                             (using ctx: Quotes): Expr[ZIO[R, E, TestResult]] = {
    import quotes.reflect._
    val (path, line) = location(ctx)
    val srcLocation = s"$path:$line"
    '{_root_.zio.test.CompileVariants.assertMProxy($effect, ${Expr(srcLocation)})($assertion)}
  }

  // inline def assert[A](inline value: => A)(inline assertion: Assertion[A]): TestResult = ${Macros.assert_impl('value)('assertion)}
  def assert_impl[A](value: Expr[A])(assertion: Expr[Assertion[A]])(using ctx: Quotes, tp: Type[A]): Expr[TestResult] = {
    import quotes.reflect._
    val (path, line) = location(ctx)
    val code = showExpr(value)
    val srcLocation = s"$path:$line"
    '{_root_.zio.test.CompileVariants.assertProxy($value, ${Expr(code)}, ${Expr(srcLocation)})($assertion)}
  }


  def showExpr[A](expr: Expr[A])(using ctx: Quotes): String = {
    import quotes.reflect._
    expr.asTerm.pos.sourceCode.get
  }

  def sourceLocation_impl(using ctx: Quotes): Expr[SourceLocation] = {
    import quotes.reflect._
    val (path, line) = location(ctx)
    '{SourceLocation(${Expr(path)}, ${Expr(line)})}
  }

  def sourcePath_impl(using ctx: Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(Position.ofMacroExpansion.sourceFile.jpath.toString)
  }

  def showExpression_impl[A](value: Expr[A])(using ctx: Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(showExpr(value))
  }
}



