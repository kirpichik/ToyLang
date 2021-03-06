package toy.lang.parser

import com.codecommit.gll.{LineStream, RegexParsers, Result}

object Parser extends RegexParsers {

  private lazy val number: Parser[NumberLit] = """\d+""".r ^^ { n => NumberLit(n.toInt) }

  private lazy val string: Parser[StringLit] = """"([^"]|\\")*"""".r ^^ { s =>
    StringLit(s.replace("\\\"", "\"").substring(1, s.length - 1))
  }

  private lazy val keywords: TerminalParser[String] = """(if|then|else|fi|while|do|done|print)""".r

  private lazy val ident: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r \ keywords

  private lazy val program: Parser[List[Expression]] = rep1(expr) <~ "\\z".r

  private lazy val expr: Parser[Expression] =
      "(" ~> expr <~ ")" |
      binaryOperators |
      ifExpression |
      whileExpression |
      eqVar |
      print |
      number |
      string |
      ident ^^ { i => IdentLit(i) }

  private lazy val ifExpression: Parser[IfExpr] =
    "if" ~ expr ~ "then" ~
      rep1(expr) ~
    "else" ~
      rep1(expr) ~
    "fi" ^^ {
      (_, predicate,  _, body, _, elseBody, _) => IfExpr(predicate, body, elseBody)
    }

  private lazy val whileExpression: Parser[WhileExpr] =
    "while" ~ expr ~ "do" ~
      rep1(expr) ~
    "done" ^^ {
      (_, predicate, _, body, _) => WhileExpr(predicate, body)
    }

  private lazy val operators: Parser[String] = "*" | "/" | "+" | "-" | "<" | ">" | ">=" | "<=" | "==" | "!="

  private lazy val binaryOperators: Parser[BinaryOperationExpr] =
    expr ~ operators ~ expr ^^ { (left, op, right) => BinaryOperationExpr(op, left, right) }

  private lazy val eqVar: Parser[EqExpr] = ident ~ "=" ~ expr ^^ { (name, _, value) => EqExpr(name, value) }

  private lazy val print: Parser[PrintExpr] = "print" ~ expr ^^ { (_, value) => PrintExpr(value) }

  def apply(stream: LineStream): Stream[Result[List[Expression]]] = program(stream)

  def apply(code: String): Stream[Result[List[Expression]]] = program(code)

}
