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
      env("++/2") = TypingScheme(Map.empty, FunctionType(List(ListType(VarType(0)), ListType(VarType(0))), ListType(VarType(0))))
      env("--/2") = TypingScheme(Map.empty, FunctionType(List(ListType(VarType(0)), ListType(VarType(0))), ListType(VarType(0))))
      for (tree@FunTree(Some(name), clauses) <- analyzer.getResult) {
        try {
          val arity = clauses(0).args.size
          val scheme = tree.check_+(env.toMap)
          env(s"$name/$arity") = scheme
          println(s"$name/$arity:${TypingScheme.show(scheme.simplify)}")
        } catch {
          case e: Throwable => println(e)
        }
      }
    }
  }
}
