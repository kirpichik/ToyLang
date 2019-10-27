package toy.lang.parser

import com.codecommit.gll.{RegexParsers, Result}

object Parser extends RegexParsers {

  def number: Parser[NumberLit] = """\d+""".r ^^ { n => NumberLit(n.toInt) }

  def string: Parser[StringLit] = """"([^"]|\\")*"""".r ^^ { s =>
    StringLit(s.replace("\\\"", "\"").substring(1, s.length - 1))
  }

  def keywords: TerminalParser[String] = """(if|then|else|fi|while|do|done|print)""".r

  def ident: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r \ keywords

  lazy val program: Parser[List[Expression]] = rep1(expr) <~ "\\z".r

  lazy val expr: Parser[Expression] =
      "(" ~> expr <~ ")" |
      binaryOperators |
      ifExpression |
      whileExpression |
      eqVar |
      print |
      number |
      string |
      ident ^^ { i => IdentLit(i) }

  lazy val ifExpression: Parser[IfExpr] =
    "if" ~ expr ~ "then" ~
      rep1(expr) ~
    "else" ~
      rep1(expr) ~
    "fi" ^^ {
      (_, predicate,  _, body, _, elseBody, _) => IfExpr(predicate, body, elseBody)
    }

  lazy val whileExpression: Parser[WhileExpr] =
    "while" ~ expr ~ "do" ~
      rep1(expr) ~
    "done" ^^ {
      (_, predicate, _, body, _) => WhileExpr(predicate, body)
    }

  def operators: Parser[String] = "*" | "/" | "+" | "-" | "<" | ">" | ">=" | "<=" | "==" | "!="

  lazy val binaryOperators: Parser[BinaryOperationExpr] =
    expr ~ operators ~ expr ^^ { (left, op, right) => BinaryOperationExpr(op, left, right) }

  lazy val eqVar: Parser[EqExpr] = ident ~ "=" ~ expr ^^ { (name, _, value) => EqExpr(name, value) }

  lazy val print: Parser[PrintExpr] = "print" ~ expr ^^ { (_, value) => PrintExpr(value) }

  def apply(code: String): Stream[Result[List[Expression]]] = program(code)

}
