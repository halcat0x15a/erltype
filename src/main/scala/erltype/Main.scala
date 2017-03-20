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
      val analyzer = new Analyzer
      parser.addParseListener(analyzer)
      parser.forms()
      val env = HashMap.empty[String, TypingScheme[ErlType[Plus]]]
      for (tree@FunTree(Some(name), _) <- analyzer.getResult) {
        try {
          val scheme = tree.check_+(env.toMap).simplify
          env(name) = scheme
          println(s"$name:$scheme")
        } catch {
          case e: Throwable => println(e.getMessage)
        }
      }
    }
  }
}
