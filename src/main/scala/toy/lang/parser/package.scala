package toy.lang

package object parser {

  sealed trait Expression

  case class StringLit(value: String) extends Expression
  case class NumberLit(value: Int) extends Expression
  case class IdentLit(name: String) extends Expression

  case class BinaryOperationExpr(op: String, left: Expression, right: Expression) extends Expression

  case class EqExpr(name: String, expr: Expression) extends Expression

  case class IfExpr(predicate: Expression, body: List[Expression], elseBody: List[Expression]) extends Expression
  case class WhileExpr(predicate: Expression, body: List[Expression]) extends Expression

  case class PrintExpr(expr: Expression) extends Expression

}
