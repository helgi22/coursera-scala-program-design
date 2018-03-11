package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (refName, exprSignal) <- namedExpressions
    } yield (refName, Signal(eval(exprSignal(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double =
    expr match {
      case Literal(v) => v
      case Ref(v) => {
        val e = getReferenceExpr(v, references)
        eval(e, references - v)
      }
      case Plus(l, r) => eval(l, references) + eval(r, references)
      case Minus(l, r) => eval(l, references) - eval(r, references)
      case Times(l, r) => eval(l, references) * eval(r, references)
      case Divide(l, r) => eval(l, references) / eval(r, references)
    }


  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
