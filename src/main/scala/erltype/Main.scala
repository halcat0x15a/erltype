package erltype

import java.nio.file.Files
import java.nio.file.Paths
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap

object Main extends App {
  for (name <- args.headOption) {
    val path = Paths.get(name)
    if (Files.exists(path)) {
      val parser = new ErlangParser(new CommonTokenStream(new ErlangLexer(new ANTLRInputStream(Files.newInputStream(path)))))
      val env = new HashMap[String, ErlType]
      env("*") = ErlFunction(List(ErlType.Number, ErlType.Number), ErlType.Number)
      env("-") = ErlFunction(List(ErlType.Number, ErlType.Number), ErlType.Number)
      env(">") = ErlFunction(List(ErlType.Number, ErlType.Number), ErlType.Boolean)
      parser.addParseListener(new ErlTypeListener(env))
      try {
        parser.forms()
        env.foreach {
          case (key, value) => println(s"$key : ${value.display}")
        }
      } catch {
        case ErlTypeListenerException(message) => println(message)
      }
    }
  }
}
