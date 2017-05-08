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
      val env = HashMap.empty[String, TypingScheme[Type[Pos]]]
      env("++/2") = {
        val a = fresh
        TypingScheme(Map.empty, FunType(Vector(ListType(VarType(a)), ListType(VarType(a))), ListType(VarType(a))))
      }
      env("--/2") = {
        val a = fresh
        TypingScheme(Map.empty, FunType(Vector(ListType(VarType(a)), ListType(VarType(a))), ListType(VarType(a))))
      }
      env("-/2") = TypingScheme(Map.empty, FunType(Vector(IntType(), IntType()), IntType()))
      env(">/2") = TypingScheme(Map.empty, FunType(Vector(IntType(), IntType()), BooleanType))
      env("is_list/1") = TypingScheme(Map.empty, FunType[Pos](Vector(TopType), BooleanType))
      for (tree@FunTree(Some(name), clauses) <- analyzer.getResult) {
        try {
          val arity = clauses(0).args.size
          val scheme = tree.check_+(env.toMap)
          env(s"$name/$arity") = scheme.simplify
          println(s"$name/$arity:${scheme.pretty.show}")
        } catch {
          case e: Throwable =>
            //println(e)
            e.printStackTrace
        }
      }
    }
  }
}
