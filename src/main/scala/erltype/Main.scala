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
      env("++/2") = TypingScheme(Map.empty, ErlFunction(List(ErlList(ErlVar(0)), ErlList(ErlVar(0))), ErlList(ErlVar(0))))
      env("--/2") = TypingScheme(Map.empty, ErlFunction(List(ErlList(ErlVar(0)), ErlList(ErlVar(0))), ErlList(ErlVar(0))))
      for (tree@FunTree(clauses) <- analyzer.getResult) {
        try {
          val Some(name) = clauses(0).name
          val arity = clauses(0).args.size
          val scheme = tree.check_+(env.toMap).simplify
          env(s"$name/$arity") = scheme
          println(s"$name/$arity:$scheme")
        } catch {
          case e: Throwable => println(e.getMessage)
        }
      }
    }
  }
}
