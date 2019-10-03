package toy.lang

package object parser {

  sealed abstract class Expression()

  case class NumberLit(number: Int) extends Expression
  case class StringLit(string: String) extends Expression
  case class IdentLit(ident: String) extends Expression

  case class BinaryOperation(op: String, left: Expression, right: Expression) extends Expression

  case class Eq(ident: String, expr: Expression) extends Expression
  case class Definition(typename: String, ident: String, expr: Expression) extends Expression

  case class IfExpr(predicate: Expression, body: Expression, elseBody: Expression) extends Expression
  case class WhileExpr(predicate: Expression, body: Expression) extends Expression

  case class Print(expr: Expression) extends Expression
}
