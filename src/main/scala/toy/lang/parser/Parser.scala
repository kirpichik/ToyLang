package toy.lang.parser

import toy.lang.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers {

  def number: Parser[NumberLit] = """[0-9]+""".r ^^ { n => NumberLit(n.toInt) }
  def string: Parser[StringLit] = """"([^"]|\\")*"""".r ^^ { s =>
    StringLit(s.replace("\\\"", "\"").substring(1, s.length - 1))
  }

  def ident: Parser[String] = """[a-zA-Z][0-9_a-zA-Z]*""".r ^^ { _.toString }

  def keyword(word: String): Parser[String] = word ^^ { _.toString }

  lazy val program: PackratParser[Seq[Expression]] =
    (expr ~ program ^^ { case e ~ p => Seq(e) ++ p }) ||| (expr ^^ { e => Seq(e)})

  lazy val expr: PackratParser[Expression] =
      "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e } |
      binaryOperators |
      ifExpression |
      whileExpression |
      eqVar |
      definition |
      print |
      number |
      string |
      ident ^^ { IdentLit }

  def ifExpression: Parser[IfExpr] =
    keyword("if") ~ expr ~ keyword("then") ~
      expr ~
    keyword("else") ~
      expr ~
    keyword("fi") ^^ {
      case _ ~ predicate ~ _ ~ body ~ _ ~ elseBody ~ _ => IfExpr(predicate, body, elseBody)
    }

  def whileExpression: Parser[WhileExpr] =
    keyword("while") ~ expr ~ keyword("do") ~
      expr ~
    keyword("done") ^^ {
      case _ ~ predicate ~ _ ~ body ~ _ => WhileExpr(predicate, body)
    }

  def operators: Parser[String] = "*" | "/" | "+" | "-" | "<" | ">" | ">=" | "<=" | "=="

  lazy val binaryOperators: PackratParser[BinaryOperation] =
    expr ~ operators ~ expr ^^ { case a ~ op ~ b => BinaryOperation(op, a, b) }

  def eqVar: Parser[Eq] = ident ~ "=" ~ expr ^^ { case name ~ _ ~ value => Eq(name, value) }

  def definition: Parser[Definition] =
    ident ~ ":" ~ ident ~ "=" ~ expr ^^ { case name ~ _ ~ typename ~ _ ~ value => Definition(typename, name, value) }

  def print: Parser[Print] = keyword("print") ~ expr ^^ { case _ ~ value => Print(value) }

  def apply(code: String): parser.Parser.ParseResult[Seq[Expression]] =
    program(new PackratReader(new CharSequenceReader(code)))

}
