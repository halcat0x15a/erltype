package erltype

import java.nio.file.Files
import java.nio.file.Paths
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import scala.collection.mutable.HashMap

object Main extends App {
  for (name <- args.headOption) {
    val path = Paths.get(name)
    if (Files.exists(path)) {
      val parser = new ErlangParser(new CommonTokenStream(new ErlangLexer(new ANTLRInputStream(Files.newInputStream(path)))))
      val env = HashMap.empty[String, TypingScheme[ErlType[Plus]]]
      parser.addParseListener(new ErlangBaseListener {
        override def exitFunction(ctx: ErlangParser.FunctionContext): Unit = {
          try {
            val scheme = ctx.check_+(env.toMap)
            val name = ctx.functionClause.get(0).tokAtom.getText
            env(name) = scheme
            println(s"$name:$scheme")
          } catch {
            case e: Throwable => println(e.getMessage)
          }
        }
      })
      parser.forms()
    }
  }
}
