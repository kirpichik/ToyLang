
import java.nio.file.{Files, Paths}

import toy.lang.analysis.TypeAnalysis
import toy.lang.generator.Generator
import toy.lang.parser.Parser

object Main {

  def main(args: Array[String]): Unit = {
    Parser(
      """if 0 then
        |print "true"
        |else
        |print "false"
        |fi
        |
        |print 123
        |
        |while 0 do
        |print "no"
        |done
        |
        |while 1 do
        |print "yes!"
        |done
        |""".stripMargin)
      .map(TypeAnalysis.typedProgram)
      .map(Generator.generateProgram)
      .map(Files.write(Paths.get("ToyLangClass.class"), _))
  }
}
