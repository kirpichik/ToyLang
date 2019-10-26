package toy.lang

package object analysis {

  sealed abstract class ExpressionType

  case object IntType extends ExpressionType
  case object StringType extends ExpressionType

  sealed abstract class TypedExpression(val exprType: ExpressionType)

  case class TypedNumberExpr(number: Int) extends TypedExpression(IntType)
  case class TypedStringExpr(string: String) extends TypedExpression(StringType)
  case class TypedIdentExpr(ident: String, identType: ExpressionType) extends TypedExpression(identType)

  // left.exprType and right.exprType must be the same
  case class TypedBinaryOperationExpr(op: String, left: TypedExpression, right: TypedExpression) extends TypedExpression(left.exprType)

  // expr.exprType must be the same as definition type
  case class TypedEqExpr(ident: String, expr: TypedExpression) extends TypedExpression(expr.exprType)
  case class TypedDefinitionExpr(defType: ExpressionType, ident: String, expr: TypedExpression) extends TypedExpression(defType)

  case class TypedCodeBlock(block: Seq[TypedExpression], summaryType: ExpressionType)

  // body.exprType and elseBody.exprType must be the same and predicate.exprType must be IntType
  case class TypedIfExpr(predicate: TypedExpression, body: TypedCodeBlock, elseBody: TypedCodeBlock) extends TypedExpression(body.summaryType)
  case class TypedWhileExpr(predicate: TypedExpression, body: TypedCodeBlock) extends TypedExpression(body.summaryType)

  case class TypedPrintExpr(expr: TypedExpression) extends TypedExpression(expr.exprType)

  sealed abstract class Constant
  case class IntConstant(number: Int) extends Constant
  case class StringConstant(string: String) extends Constant

}
