package toy.lang.analysis

import toy.lang.parser._

import scala.annotation.tailrec

object TypeAnalysis {

  private type Scope = Map[String, ExpressionType]

  class TypeCheckException(reason: String) extends RuntimeException(reason)
  class UnknownIdentifierException(ident: String) extends RuntimeException(ident)

  def apply(program: Program): TypedProgram = typedProgram(program)

  def typedProgram(program: Program): TypedProgram = {
    @tailrec
    def expressionsIterator(program: Program, scope: Scope, acc: TypedProgram): TypedProgram = {
      if (program.isEmpty)
        acc
      else {
        val (expr, newScope) = typedExpression(program.head, scope)
        expressionsIterator(program.tail, newScope, acc :+ expr)
      }
    }

    expressionsIterator(program, Map.empty, Seq.empty)
  }

  def typedExpression(expr: Expression, scope: Scope): (TypedExpression, Scope) = expr match {
    case NumberLit(number) => (TypedNumberExpr(number), scope)

    case StringLit(string) => (TypedStringExpr(string), scope)

    case IdentLit(ident) => (TypedIdentExpr(ident, scope(ident)), scope)

    case BinaryOperation(op, left, right) =>
      val (typedLeft, _) = typedExpression(left, scope)
      val (typedRight, _) = typedExpression(right, scope)
      if (typedLeft.exprType != typedRight.exprType)
        throw new TypeCheckException(s"Left ${typedLeft.exprType} != Right ${typedRight.exprType} in operation $op")
      if (typedLeft.exprType == StringType && op != "+" && op != "==" && op != "!=")
        throw new TypeCheckException(s"Operation $op cannot be applied to String type")
      (TypedBinaryOperationExpr(op, typedLeft, typedRight), scope)

    case Eq(ident, expr) =>
      if (!scope.contains(ident))
        throw new UnknownIdentifierException(ident)
      val (typedExpr, _) = typedExpression(expr, scope)
      if (scope(ident) != typedExpr.exprType)
        throw new TypeCheckException(s"Variable $ident has type ${scope(ident)}, but right expression has type ${typedExpr.exprType}")
      (TypedEqExpr(ident, typedExpr), scope)

    case Definition(typename, ident, expr) =>
      val defType = typename match {
        case "String" => StringType
        case "Int" => IntType
      }
      val (typedExpr, newScope) = typedExpression(expr, scope)
      if (typedExpr.exprType != defType)
        throw new TypeCheckException(s"Definition type $defType differ than right expression type ${typedExpr.exprType}")
      (TypedDefinitionExpr(defType, ident, typedExpr), newScope)

    case IfExpr(predicate, body, elseBody) =>
      val (predicateExpr, _) = typedExpression(predicate, scope)
      if (predicateExpr.exprType != IntType)
        throw new TypeCheckException(s"Predicate type ${predicateExpr.exprType} differ than $IntType")
      val (bodyExpr, _) = typedExpression(body, scope)
      val (elseBodyExpr, _) = typedExpression(elseBody, scope)
      if (bodyExpr.exprType != elseBodyExpr.exprType)
        throw new TypeCheckException(s"If body type ${bodyExpr.exprType} differ than else body type ${elseBodyExpr.exprType}")
      (TypedIfExpr(predicateExpr, bodyExpr, elseBodyExpr), scope)

    case WhileExpr(predicate, body) =>
      val (predicateExpr, _) = typedExpression(predicate, scope)
      if (predicateExpr.exprType != IntType)
        throw new TypeCheckException(s"Predicate type ${predicateExpr.exprType} differ than $IntType")
      (TypedWhileExpr(predicateExpr, typedExpression(body, scope)._1), scope)

    case Print(expr) =>
      (TypedPrintExpr(typedExpression(expr, scope)._1), scope)
  }

}
