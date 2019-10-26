package toy.lang.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers {

  var keywords = Set("if", "then", "else", "fi", "while", "do", "done", "print")

  def number: Parser[NumberLit] = """\d+""".r ^^ { n => NumberLit(n.toInt) }

  def string: Parser[StringLit] = """"([^"]|\\")*"""".r ^^ { s =>
    StringLit(s.replace("\\\"", "\"").substring(1, s.length - 1))
  }

  def ident: Parser[String] = not(keywords.map{ Parser[String](_) }.reduce(_ | _)) ~> """[a-zA-Z][a-zA-Z0-9_]*""".r

  lazy val program: PackratParser[List[Expression]] = rep1(expr) <~ "\\z".r

  lazy val expr: PackratParser[Expression] =
      "(" ~> expr <~ ")" |
      binaryOperators |
      ifExpression |
      whileExpression |
      eqVar |
      print |
      number |
      string |
      ident ^^ { IdentLit }

  lazy val ifExpression: PackratParser[IfExpr] =
    "if" ~ expr ~ "then" ~
      rep1(expr) ~
    "else" ~
      rep1(expr) ~
    "fi" ^^ {
      case _ ~ predicate ~ _ ~ body ~ _ ~ elseBody ~ _ => IfExpr(predicate, body, elseBody)
    }

  lazy val whileExpression: PackratParser[WhileExpr] =
    "while" ~ expr ~ "do" ~
      rep1(expr) ~
    "done" ^^ {
      case _ ~ predicate ~ _ ~ body ~ _ => WhileExpr(predicate, body)
    }

  def operators: Parser[String] = "*" | "/" | "+" | "-" | "<" | ">" | ">=" | "<=" | "==" | "!="

  lazy val binaryOperators: PackratParser[BinaryOperationExpr] =
    expr ~ operators ~ expr ^^ { case left ~ op ~ right => BinaryOperationExpr(op, left, right) }

  lazy val eqVar: PackratParser[EqExpr] = ident ~ "=" ~ expr ^^ { case name ~ _ ~ value => EqExpr(name, value) }

  lazy val print: PackratParser[PrintExpr] = "print" ~ expr ^^ { case _ ~ value => PrintExpr(value) }

  def apply(code: String): ParseResult[List[Expression]] =
    parseAll(phrase(program), new PackratReader[Char](new CharSequenceReader(code)))

}
