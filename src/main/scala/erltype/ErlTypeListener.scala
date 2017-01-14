package erltype

import scala.collection.mutable.HashMap

class ErlTypeListener(global: HashMap[String, ErlType]) extends ErlangBaseListener {

  override def exitFunctionClause(ctx: ErlangParser.FunctionClauseContext): Unit = {
    if (!ctx.getText.isEmpty) {
      val name = ctx.tokAtom.getText
      ctx.check(global.toMap - name).flatMap { (env, typ) =>
        Success(env, ErlTyper.union(global.getOrElse(name, ErlVar(name)), ErlTyper.resolve(env, typ)))
      } match {
        case Success(_, typ) => global(name) = typ
        case Failure(message) => throw ErlTypeListenerException(message)
      }
    }
  }

}

case class ErlTypeListenerException(message: String) extends Exception(message)
