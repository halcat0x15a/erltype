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
      env("*") = ErlFunction(List(ErlInteger, ErlInteger), ErlInteger)
      env("-") = ErlFunction(List(ErlInteger, ErlInteger), ErlInteger)
      parser.addParseListener(new ErlTypeListener(env))
      parser.forms()
      println(env)
    }
  }
}
