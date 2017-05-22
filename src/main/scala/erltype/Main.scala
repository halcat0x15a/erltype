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
      val listener = new ParseListener
      parser.addParseListener(listener)
      parser.forms()
      val env = HashMap.empty[String, TypingScheme[Type[Pos]]]
      env("++/2") = {
        val a = fresh
        TypingScheme(Map.empty, FunctionType(List(ListType(VarType(a)), ListType(VarType(a))), ListType(VarType(a))))
      }
      env("--/2") = {
        val a = fresh
        TypingScheme(Map.empty, FunctionType(List(ListType(VarType(a)), ListType(VarType(a))), ListType(VarType(a))))
      }
      env("+/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), IntType()))
      env("-/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), IntType()))
      env("*/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), IntType()))
      env("div/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), IntType()))
      env(">/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), BooleanType))
      env("</2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), BooleanType))
      env(">=/2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), BooleanType))
      env("=</2") = TypingScheme(Map.empty, FunctionType(List(IntType(), IntType()), BooleanType))
      env("=:=/2") = TypingScheme(Map.empty, FunctionType[Pos](List(TopType, TopType), BooleanType))
      env("is_integer/1") = TypingScheme(Map.empty, FunctionType[Pos](List(TopType), BooleanType))
      env("is_list/1") = TypingScheme(Map.empty, FunctionType[Pos](List(TopType), BooleanType))
      env("is_function/2") = TypingScheme(Map.empty, FunctionType[Pos](List(TopType, IntType()), BooleanType))
      env("andalso/2") = TypingScheme(Map.empty, FunctionType[Pos](List(BooleanType, BooleanType), BooleanType))
      env("length/1") = TypingScheme(Map.empty, FunctionType[Pos](List(ListType(TopType)), IntType()))
      for (tree@FunTree(clauses) <- listener.getResult) {
        try {
          val Some(name) = clauses(0).name
          val arity = clauses(0).args.size
          val scheme = tree.check(env.toMap)
          env(s"$name/$arity") = TypingScheme.simplify(scheme)
          println(s"$name/$arity:${scheme.pretty.show}")
        } catch {
          case e: Throwable =>
            e.printStackTrace
            //println(e)
        }
      }
    }
  }
}
