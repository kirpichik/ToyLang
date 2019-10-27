
import java.io.InputStream
import java.nio.file.{Files, Paths}

import com.codecommit.gll.{Failure, LineStream, Success}
import org.rogach.scallop.ScallopConf
import toy.lang.analysis.TypeAnalysis
import toy.lang.generator.Generator
import toy.lang.parser.{Expression, Parser}

import scala.io.Source

object Main {

  class Config(args: Seq[String]) extends ScallopConf(args) {
    val output = opt[String](default = Some("ToyLangClass"))
    val input = trailArg[String]()
    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new Config(args)

    compileAndExec(conf.input(), conf.output())
  }

  def compileAndExec(input: String, output: String): Unit = {
    val results = Parser(LineStream(Source.fromFile(input)))
    if (results exists { _.isInstanceOf[Success[List[Expression]]] }) {
      (for (Success(tree, _) <- results) yield tree)
        .map(TypeAnalysis.apply)
        .map(Generator.generateProgram(_, output))
        .map(bytes => Files.write(Paths.get(output + ".class"), bytes))
    } else {
      val sorted = results.toList sortWith { _.tail.length < _.tail.length }
      val length = sorted.head.tail.length

      for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
        val pattern = "  error:%%d: %s%n    %%s%n    %%s%n".format(msg)
        tail.printError(pattern)(System.err)
      }
    }

    val disassembler = Runtime.getRuntime.exec(s"javap -v $output.class")
    val exec = Runtime.getRuntime.exec(s"java $output")

    println("====== Execution: =========")
    outputPrinter(exec.getInputStream)
    println("====== Disassembler: ======")
    outputPrinter(disassembler.getInputStream)
  }

  def outputPrinter(stream: InputStream): Unit = Iterator
    .continually(stream.read)
    .takeWhile(_ != -1)
    .foreach(System.out.write)
}
