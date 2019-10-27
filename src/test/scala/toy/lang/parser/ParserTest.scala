package toy.lang.parser

import com.codecommit.gll.Success
import org.scalatest.{Assertion, FunSuite}

class ParserTest extends FunSuite {

  def parse(code: String): List[Expression] = (for (Success(tree, _) <- Parser(code)) yield tree).head

  def assertSingle(expr: Expression): Any => Assertion = assertResult(List(expr))

  test("number") {
    assertSingle(NumberLit(0))(parse("0"))
    assertSingle(NumberLit(42))(parse("42"))
  }

  test("string") {
    assertSingle(StringLit(""))(parse(""" "" """))
    assertSingle(StringLit("abc"))(parse(""" "abc" """))
  }

  test("ident") {
    assertSingle(IdentLit("a"))(parse("a"))
    assertSingle(IdentLit("test"))(parse("test"))
  }

  test("binaryOperators") {
    Seq("+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=").foreach { op =>
      assertSingle(BinaryOperationExpr(op, NumberLit(2), NumberLit(2)))(parse(s"2 $op 2"))
      assertSingle(BinaryOperationExpr(op, IdentLit("a"), IdentLit("b")))(parse(s"a $op b"))
      assertSingle(BinaryOperationExpr(op, IdentLit("a"), NumberLit(2)))(parse(s"a $op 2"))
      assertSingle(BinaryOperationExpr(op, NumberLit(2), IdentLit("b")))(parse(s"2 $op b"))
      assertSingle(BinaryOperationExpr(op, IdentLit("a"), StringLit("string")))(parse(s""" a $op "string" """))
      assertSingle(BinaryOperationExpr(op, StringLit("string"), IdentLit("b")))(parse(s""" "string" $op b """))
    }
  }

  test("print") {
    assertSingle(PrintExpr(NumberLit(2)))(parse("print 2"))
    assertSingle(PrintExpr(StringLit("test")))(parse("print \"test\""))
    assertSingle(PrintExpr(IdentLit("a")))(parse("print a"))
  }

  test("eqVar") {
    assertSingle(EqExpr("a", NumberLit(0)))(parse("a = 0"))
    assertSingle(EqExpr("test", StringLit("new")))(parse("test = \"new\""))
  }

  test("if") {
    assertSingle(IfExpr(
      NumberLit(0),
      List(NumberLit(1)),
      List(NumberLit(0)))
    )(parse("if 0 then 1 else 0 fi"))

    assertSingle(IfExpr(
      NumberLit(0),
      List(PrintExpr(StringLit("true")), NumberLit(0)),
      List(PrintExpr(StringLit("false")), NumberLit(1)))
    )(parse("if 0 then print \"true\"\n0 else print \"false\"\n1 fi"))
  }

  test("while") {
    assertSingle(WhileExpr(
      NumberLit(0),
      List(PrintExpr(StringLit("true"))))
    )(parse("while 0 do print \"true\" done"))

    assertSingle(WhileExpr(
      NumberLit(0),
      List(PrintExpr(StringLit("true")), NumberLit(0)))
    )(parse("while 0 do print \"true\"\n0 done"))
  }

  test("program") {
    assertResult(List(NumberLit(0), StringLit("test")))(parse("0\n\"test\""))
  }

}
