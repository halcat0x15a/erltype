package erltype

import java.nio.file.Files
import java.nio.file.Paths
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

object Main extends App {
  for (name <- args.headOption) {
    val path = Paths.get(name)
    if (Files.exists(path)) {
      val parser = new ErlangParser(new CommonTokenStream(new ErlangLexer(new ANTLRInputStream(Files.newInputStream(path)))))
      parser.addParseListener(new ErlangBaseListener {
        override def exitFunctionClause(ctx: ErlangParser.FunctionClauseContext): Unit = {
          println(ErlTyper.show(ctx.check(Map.empty)))
        }
      })
      parser.forms()
    }
  }
}
