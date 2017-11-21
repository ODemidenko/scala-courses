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
    //Refs to other variables could cause cyclic dependencies (e.g., a = b + 1 and b = 2 * a.
    // Such cyclic dependencies are considered as errors
    // (failing to detect this will cause infinite loops).

    //нужно для каждого Expr понимать как называется текущая вычисляемая переменная и добавлять её в стек


    //то есть нужно сигналы со значенияеми поставить в зависимость от сигналов с выражениями
    for ((name,value)<-namedExpressions) yield name->Signal(evaluateVar(name,value(),namedExpressions))

  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v)=>v
      case Ref(name)=>evaluateVar(name,getReferenceExpr(name,references),references)
      case Plus(a,b)=>eval(a,references)+eval(b,references)
      case Minus(a,b)=>eval(a,references)-eval(b,references)
      case Times(a,b)=>eval(a,references)*eval(b,references)
      case Divide(a,b)=>eval(a,references)/eval(b,references)
    }
  }

  var variables=List[String]()
  def evaluateVar(name:String,expr:Expr,references: Map[String, Signal[Expr]]):Double =
  {
    variables.find(_==name) match {
      case None=>{
        variables=name::variables
        val evaluatedExpr=eval(expr,references)
        variables=variables.tail
        evaluatedExpr
      }
      case found=>Double.NaN
  }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
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
class StackableExpression(stack:List[Expr]) {

}
