package toy.lang.generator

import jdk.internal.org.objectweb.asm.commons.InstructionAdapter
import jdk.internal.org.objectweb.asm.{ClassWriter, Label, Opcodes}
import toy.lang.analysis._

import scala.annotation.tailrec

object Generator {

  class Scope(map: Map[String, List[Int]], last: Int) {
    def resolve(name: String): Int = map(name).head

    def contains(name: String): Boolean = map.contains(name)

    def define(name: String): Scope = new Scope(map + (name -> (last :: map.getOrElse(name, Nil))), last + 1)
  }

  def generateProgram(program: TypedCodeBlock): Array[Byte] = {
    val classWriter: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriter.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC, "ToyLangClass", null, "java/lang/Object", null)
    val methodVisitor = new InstructionAdapter(classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null))
    methodVisitor.visitCode()

    generateCodeBlock(program.block, methodVisitor, new Scope(Map.empty, 0))

    methodVisitor.visitInsn(Opcodes.RETURN)
    methodVisitor.visitMaxs(100, 100) // TODO
    methodVisitor.visitEnd()
    classWriter.visitEnd()

    classWriter.toByteArray
  }

  @tailrec
  def generateCodeBlock(program: List[TypedExpression], method: InstructionAdapter, scope: Scope): Unit = {
    if (program.nonEmpty)
      generateCodeBlock(program.tail, method, generateExpression(program.head, method, scope))
  }

  def generateExpression(expr: TypedExpression, body: InstructionAdapter, scope: Scope): Scope = {
    expr match {
      case TypedNumberExpr(number) =>
        body.iconst(number)
      case TypedStringExpr(string) =>
        body.aconst(string)
      case TypedIdentExpr(ident, identType) =>
        body.visitVarInsn(identType match {
          case IntType => Opcodes.ILOAD
          case StringType => Opcodes.ALOAD
        }, scope.resolve(ident))
      case TypedBinaryOperationExpr(op, left, right, _) =>
        generateExpression(left, body, scope)
        generateExpression(right, body, scope)
        left.exprType match {
          case IntType => generateIntBinaryOperation(op, body)
          case StringType => generateStringBinaryOperation(op, body)
        }
      case TypedEqExpr(ident, expr) =>
        generateExpression(expr, body, scope)
        val newScope = scope.define(ident)
        body.visitVarInsn(expr.exprType match {
          case IntType => Opcodes.ISTORE
          case StringType => Opcodes.ASTORE
        }, newScope.resolve(ident))
        return newScope
      case TypedIfExpr(predicate, ifBody, elseBody) =>
        generateIfExpr(predicate, ifBody, elseBody, body, scope)
      case TypedWhileExpr(predicate, whileBody) =>
        generateWhileExpr(predicate, whileBody, body, scope)
      case TypedPrintExpr(expr) =>
        body.getstatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        generateExpression(expr, body, scope)
        body.invokevirtual("java/io/PrintStream", "println", expr.exprType match {
          case IntType => "(I)V"
          case StringType => "(Ljava/lang/String;)V"
        }, false)
    }
    scope
  }

  val arithmeticOperators: Map[String, Int] = Map(
    "+" -> Opcodes.IADD,
    "-" -> Opcodes.ISUB,
    "*" -> Opcodes.IMUL,
    "/" -> Opcodes.IDIV
  )
  val comparingOperators: Map[String, Int] = Map(
    "<" -> Opcodes.IF_ICMPGE,
    ">" -> Opcodes.IF_ICMPLE,
    ">=" -> Opcodes.IF_ICMPLT,
    "<=" -> Opcodes.IF_ICMPGT,
    "==" -> Opcodes.IF_ICMPNE,
    "!=" -> Opcodes.IF_ICMPEQ
  )

  def generateIntBinaryOperation(op: String, body: InstructionAdapter): Unit =
    arithmeticOperators.get(op) match {
      case Some(code) => body.visitInsn(code)
      case None =>
        val trueLabel = new Label
        val falseLabel = new Label
        body.visitJumpInsn(comparingOperators(op), falseLabel)
        body.iconst(1)
        body.goTo(trueLabel)
        body.visitLabel(falseLabel)
        body.iconst(0)
        body.visitLabel(trueLabel)
    }

  def generateStringBinaryOperation(op: String, body: InstructionAdapter): Unit =
    if (op == "+")
      body.invokevirtual("java/lang/String", "concat", "(Ljava/lang/String;)Ljava/lang/String;", false)
    else {
      body.invokevirtual("java/lang/String", "equals", "(Ljava/lang/Object;)Z", false)
      if (op == "!=") {
        val trueLabel = new Label
        val falseLabel = new Label
        body.ifne(falseLabel)
        body.iconst(1)
        body.goTo(trueLabel)
        body.visitLabel(falseLabel)
        body.iconst(0)
        body.visitLabel(trueLabel)
      }
    }

  def generateIfExpr(predicate: TypedExpression,
                     ifBody: TypedCodeBlock,
                     elseBody: TypedCodeBlock,
                     body: InstructionAdapter,
                     scope: Scope): Unit = {
    val elseLabel = new Label
    val endIfLabel = new Label

    generateExpression(predicate, body, scope)

    body.ifeq(elseLabel)

    generateCodeBlock(ifBody.block, body, scope)

    body.goTo(endIfLabel)

    body.visitLabel(elseLabel)
    generateCodeBlock(elseBody.block, body, scope)

    body.visitLabel(endIfLabel)
  }

  def generateWhileExpr(predicate: TypedExpression,
                        whileBody: TypedCodeBlock,
                        body: InstructionAdapter,
                        scope: Scope): Unit = {
    val whileBegin = new Label
    val whileEnd = new Label

    body.visitLabel(whileBegin)
    generateExpression(predicate, body, scope)

    body.ifeq(whileEnd)

    generateCodeBlock(whileBody.block, body, scope)

    body.goTo(whileBegin)

    body.visitLabel(whileEnd)
  }

}
