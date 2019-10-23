package toy.lang.generator

import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}
import toy.lang.analysis._

import scala.annotation.tailrec

object Generator {

  def generateProgram(program: TypedProgram): Array[Byte] = {
    @tailrec
    def expressionsIterator(program: TypedProgram, method: MethodVisitor): Unit = {
      if (program.nonEmpty) {
        generateExpression(program.head, method)
        expressionsIterator(program.tail, method)
      }
    }

    val classWriter: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriter.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "ToyLangClass", null, "java/lang/Object", null)
    val methodVisitor = classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    methodVisitor.visitCode()
    //methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)

    expressionsIterator(program, methodVisitor)

    methodVisitor.visitInsn(Opcodes.RETURN)
    methodVisitor.visitMaxs(100, 100) // TODO
    methodVisitor.visitEnd()
    classWriter.visitEnd()

    classWriter.toByteArray
  }

  def generateExpression(expr: TypedExpression, body: MethodVisitor): Unit = expr match {
    case TypedNumberExpr(number) =>
      body.visitLdcInsn(number)
    case TypedStringExpr(string) =>
      body.visitLdcInsn(string)
    case TypedIdentExpr(ident, identType) =>
      // Load by name
    case TypedBinaryOperationExpr(op, left, right) =>
      generateExpression(left, body)
      generateExpression(right, body)
      left.exprType match {
        case IntType => generateIntBinaryOperation(op, body)
        case StringType => // TODO
      }
    case TypedEqExpr(ident, expr) =>
      generateExpression(expr, body)
      // Store by name
    case TypedDefinitionExpr(typename, ident, expr) =>
      generateExpression(expr, body)
      // Create var and store by name
    case TypedIfExpr(predicate, ifBody, elseBody) =>
      val elseLabel = new Label
      val endIfLabel = new Label
      generateExpression(predicate, body)
      body.visitJumpInsn(Opcodes.IFEQ, elseLabel)
      generateExpression(ifBody, body)
      body.visitJumpInsn(Opcodes.GOTO, endIfLabel)
      body.visitLabel(elseLabel)
      body.visitFrame(Opcodes.F_SAME, 0, null, 0, null)
      generateExpression(elseBody, body)
      body.visitLabel(endIfLabel)
      body.visitFrame(Opcodes.F_SAME, 0, null, 0, null)
    case TypedWhileExpr(predicate, whileBody) =>
      val whileBegin = new Label
      val whileEnd = new Label
      body.visitLabel(whileBegin)
      body.visitFrame(Opcodes.F_SAME, 0, null, 0, null)
      generateExpression(predicate, body)
      body.visitJumpInsn(Opcodes.IFEQ, whileEnd)
      generateExpression(whileBody, body)
      body.visitJumpInsn(Opcodes.GOTO, whileBegin)
      body.visitLabel(whileEnd)
      body.visitFrame(Opcodes.F_SAME, 0, null, 0, null)
    case TypedPrintExpr(expr) =>
      body.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      generateExpression(expr, body)
      body.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", expr.exprType match {
        case IntType => "(I)V"
        case StringType => "(Ljava/lang/String;)V"
      }, false)
  }

  def generateIntBinaryOperation(op: String, body: MethodVisitor): Unit =
    body.visitInsn(op match {
      case "+" => Opcodes.IADD
      case "-" => Opcodes.ISUB
      case "*" => Opcodes.IMUL
      case "/" => Opcodes.IDIV
        // TODO
      case "<" => 0
      case ">" => 0
      case ">=" => 0
      case "<=" => 0
      case "==" => 0
    })

}
