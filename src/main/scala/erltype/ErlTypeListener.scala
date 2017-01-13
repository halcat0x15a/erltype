package erltype

import scala.collection.mutable.HashMap

class ErlTypeListener(global: HashMap[String, ErlType]) extends ErlangBaseListener {

  override def exitFunctionClause(ctx: ErlangParser.FunctionClauseContext): Unit = {
    ctx.check(global.toMap).flatMap { (env, typ) =>
      val name = ctx.tokAtom.getText
      ErlTyper.unify(env, global.getOrElse(name, ErlVar(name)), typ).map { typ =>
        global(name) = typ
      }
    }
  }

}
