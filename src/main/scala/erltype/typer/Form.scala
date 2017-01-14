package erltype

package typer

import scala.collection.JavaConverters._

trait FunctionClause extends ErlTyper[ErlangParser.FunctionClauseContext] {
  def check(env: TypeEnv, ctx: ErlangParser.FunctionClauseContext) = {
    val params = Option(ctx.clauseArgs.argumentList.exprs).toList.flatMap(_.expr.asScala)
    ErlTyper.checkAll(env, params).flatMap { (env, params) =>
      Option(ctx.clauseGuard.guard).map { guard =>
        ErlTyper.checkAll(env, guard.exprs.asScala.flatMap(_.expr.asScala)).flatMap { (env, types) =>
          ErlTyper.unifyAll(env, List.fill(types.size)(ErlType.Boolean), types)
        }
      }.getOrElse(Success(env, ())).flatMap { (env, _) =>
        ErlTyper.define(env, ctx.tokAtom.getText, ErlFunction(params, ErlVar("ReturnType"))).flatMap { (env, _) =>
          ErlTyper.checkAll(env, ctx.clauseBody.exprs.expr.asScala).map { body =>
            ErlFunction(params, body.last)
          }
        }
      }
    }
  }
}
