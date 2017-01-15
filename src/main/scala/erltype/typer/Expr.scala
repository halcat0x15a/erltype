package erltype

package typer

import scala.collection.JavaConverters._

trait ExprMaxContext extends ErlTyper[ErlangParser.ExprMaxContext] {
  def check(env: TypeEnv, ctx: ErlangParser.ExprMaxContext) =
    ctx.tokVar.check(env) orElse
    ctx.atomic.check(env) orElse
    ctx.expr.check(env) orElse
    ctx.list.check(env) orElse
    ctx.listComprehension.check(env)
}

trait Expr800Context extends ErlTyper[ErlangParser.Expr800Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr800Context) = ctx.exprMax.get(0).check(env)
}

trait Expr700Context extends ErlTyper[ErlangParser.Expr700Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr700Context) = ctx.expr800.check(env) orElse ctx.functionCall.check(env)
}

trait Expr600Context extends ErlTyper[ErlangParser.Expr600Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr600Context) = {
    val result = ctx.expr700.check(env)
    Option(ctx.prefixOp).fold(result) { op =>
      env.get(op.getText).fold(Result.NotFound(op.getText): Result[ErlType]) { f =>
        result.flatMap((env, typ) => ErlTyper.application(env, f, List(typ)))
      }
    }
  }
}

trait Expr500Context extends ErlTyper[ErlangParser.Expr500Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr500Context) = ErlTyper.operations(env, ctx.expr600.asScala, Option(ctx.multOp).map(_.asScala.map(_.getText)))
}

trait Expr400Context extends ErlTyper[ErlangParser.Expr400Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr400Context) = ErlTyper.operations(env, ctx.expr500.asScala, Option(ctx.addOp).map(_.asScala.map(_.getText)))
}

trait Expr300Context extends ErlTyper[ErlangParser.Expr300Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr300Context) = ErlTyper.operations(env, ctx.expr400.asScala, Option(ctx.listOp).map(_.asScala.map(_.getText)))
}

trait Expr200Context extends ErlTyper[ErlangParser.Expr200Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr200Context) = ErlTyper.operation(env, ctx.expr300.asScala, Option(ctx.compOp).map(_.getText))
}

trait Expr160Context extends ErlTyper[ErlangParser.Expr160Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr160Context) = ErlTyper.operation(env, ctx.expr200.asScala, Some("andalso"))
}

trait Expr150Context extends ErlTyper[ErlangParser.Expr150Context] {
  def check(env: TypeEnv, ctx: ErlangParser.Expr150Context) = ErlTyper.operation(env, ctx.expr160.asScala, Some("orelse"))
}

trait ExprContext extends ErlTyper[ErlangParser.ExprContext] {
  def check(env: TypeEnv, ctx: ErlangParser.ExprContext) = ctx.expr100.expr150.get(0).check(env)
}

trait FunctionCallContext extends ErlTyper[ErlangParser.FunctionCallContext] {
  def check(env: TypeEnv, ctx: ErlangParser.FunctionCallContext) = {
    ctx.expr800.check(env).flatMap { (env, f) =>
      ErlTyper.checkAll(env, Option(ctx.argumentList.exprs).toList.flatMap(_.expr.asScala)).flatMap(ErlTyper.application(_, f, _))
    }
  }
}

trait ListContext extends ErlTyper[ErlangParser.ListContext] {
  def check(env: TypeEnv, ctx: ErlangParser.ListContext) = {
    def checkTail(env: TypeEnv, tail: ErlangParser.TailContext): Result[List[ErlType]] = {
      tail.expr.check(env).flatMap { (env, typ) =>
        Option(tail.tail).fold(Success(env, List(typ)): Result[List[ErlType]])(checkTail(env, _).map(typ :: _))
      }.orElse(Success(env, Nil))
    }
    ctx.expr.check(env).flatMap { (env, head) =>
      checkTail(env, ctx.tail).map { tail =>
        ErlList(tail.foldRight(head)(ErlTyper.union))
      }
    }.orElse(Success(env, ErlList(ErlVar("Unbound"))))
  }
}

trait ListComprehensionContext extends ErlTyper[ErlangParser.ListComprehensionContext] {
  def check(env: TypeEnv, ctx: ErlangParser.ListComprehensionContext) = {
    ctx.lcExprs.lcExpr.asScala.foldLeft(ctx.expr.check(env)) { (result, lc) =>
      result.flatMap { (env, typ) =>
        lc.expr.asScala match {
          case lhs +: rhs +: _ => lhs.check(env).flatMap { (env, lhs) =>
            rhs.check(env).flatMap { (env, rhs) => ErlTyper.unify(env, ErlList(lhs), rhs) }.map(_ => typ)
          }
          case expr +: _ => expr.check(env).flatMap(ErlTyper.unify(_, ErlType.Boolean, _)).map(_ => typ)
        }
      }
    }.map(ErlList(_))
  }
}
