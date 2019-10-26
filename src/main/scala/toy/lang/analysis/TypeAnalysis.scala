package toy.lang.analysis

import toy.lang.parser._

import scala.annotation.tailrec

object TypeAnalysis {

  class TypeCheckException(reason: String) extends RuntimeException(reason)
  class UnknownIdentifierException(ident: String) extends RuntimeException(ident)

  def apply(program: Seq[Expression]): TypedCodeBlock = typedCodeBlock(program, Map.empty)

  private type Scope = Map[String, ExpressionType]

  private def typedCodeBlock(block: Seq[Expression], scope: Scope): TypedCodeBlock = {
    @tailrec
    def expressionsIterator(block: Seq[Expression], scope: Scope, typedBlock: Seq[TypedExpression]): TypedCodeBlock = {
      block match {
        case Seq(last) =>
          val (expr, _) = typedExpression(last, scope)
          TypedCodeBlock(typedBlock :+ expr, expr.exprType)
        case seq: Seq[Expression] =>
          val (expr, newScope) = typedExpression(seq.head, scope)
          expressionsIterator(seq.tail, newScope, typedBlock :+ expr)
      }
    }

    expressionsIterator(block, scope, Seq.empty)
  }

  private def typedExpression(expr: Expression, scope: Scope): (TypedExpression, Scope) = expr match {
    case NumberLit(number) => (TypedNumberExpr(number), scope)

    case StringLit(string) => (TypedStringExpr(string), scope)

    case IdentLit(ident) => (TypedIdentExpr(ident, scope(ident)), scope)

    case BinaryOperationExpr(op, left, right) =>
      val (typedLeft, _) = typedExpression(left, scope)
      val (typedRight, _) = typedExpression(right, scope)
      if (typedLeft.exprType != typedRight.exprType)
        throw new TypeCheckException(s"Left ${typedLeft.exprType} != Right ${typedRight.exprType} in operation $op")
      if (typedLeft.exprType == StringType && op != "+" && op != "==" && op != "!=")
        throw new TypeCheckException(s"Operation $op cannot be applied to String type")
      (TypedBinaryOperationExpr(op, typedLeft, typedRight), scope)

    case EqExpr(ident, expr) =>
      val (typedExpr, _) = typedExpression(expr, scope)
      if (scope.contains(ident)) {
        if (scope(ident) != typedExpr.exprType)
          throw new TypeCheckException(s"Variable $ident has type ${scope(ident)}, " +
            s"but right expression has type ${typedExpr.exprType}")
        (TypedEqExpr(ident, typedExpr), scope)
      } else
        (TypedEqExpr(ident, typedExpr), scope + (ident -> typedExpr.exprType))

    case IfExpr(predicate, body, elseBody) =>
      val (predicateExpr, _) = typedExpression(predicate, scope)
      if (predicateExpr.exprType != IntType)
        throw new TypeCheckException(s"Predicate type ${predicateExpr.exprType} differ than $IntType")
      val bodyExpr = typedCodeBlock(body, scope)
      val elseBodyExpr = typedCodeBlock(elseBody, scope)
      if (bodyExpr.summaryType != elseBodyExpr.summaryType)
        throw new TypeCheckException(s"If body type ${bodyExpr.summaryType} differ than else body type ${elseBodyExpr.summaryType}")
      (TypedIfExpr(predicateExpr, bodyExpr, elseBodyExpr), scope)

    case WhileExpr(predicate, body) =>
      val (predicateExpr, _) = typedExpression(predicate, scope)
      if (predicateExpr.exprType != IntType)
        throw new TypeCheckException(s"Predicate type ${predicateExpr.exprType} differ than $IntType")
      (TypedWhileExpr(predicateExpr, typedCodeBlock(body, scope)), scope)

    case PrintExpr(expr) =>
      (TypedPrintExpr(typedExpression(expr, scope)._1), scope)
  }

}
