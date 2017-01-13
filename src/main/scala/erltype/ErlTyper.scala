package erltype

import scala.collection.JavaConverters._

trait ErlTyper[A] {
  def check(env: TypeEnv, ctx: A): Result[ErlType]
}

object ErlTyper {

  def unify(env: TypeEnv, param: ErlType, arg: ErlType): Result[ErlType] =
    (param, arg) match {
      case (ErlVar(name), _) => Success(env.updated(name, arg), arg)
      case (_, ErlVar(name)) => Success(env.updated(name, param), param)
      case (ErlFunction(params, ret), ErlFunction(args, body)) =>
        unifyAll(env, params, args).flatMap { (env, types) =>
          unify(env, ret, body).map(ErlFunction(types, _))
        }
      case _  if param == arg => Success(env, param)
      case _ => throw new Exception(s"expected: $param; actual: $arg")
    }

  def unifyAll(env: TypeEnv, params: List[ErlType], args: List[ErlType]): Result[List[ErlType]] =
    (params zip args).foldRight(Success(env, Nil): Result[List[ErlType]]) {
      case ((param, arg), result) =>
        result.flatMap { (env, types) =>
          unify(env, param, arg).map(_ :: types)
        }
    }

  implicit object TokChar extends ErlTyper[ErlangParser.TokCharContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokCharContext): Result[ErlType] = Success(env, ErlChar)
  }

  implicit object TokInteger extends ErlTyper[ErlangParser.TokIntegerContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokIntegerContext): Result[ErlType] = Success(env, ErlInteger)
  }

  implicit object TokFloat extends ErlTyper[ErlangParser.TokFloatContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokFloatContext): Result[ErlType] = Success(env, ErlFloat)
  }

  implicit object TokAtom extends ErlTyper[ErlangParser.TokAtomContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokAtomContext): Result[ErlType] = Success(env, ErlAtom(ctx.getText))
  }

  implicit object TokString extends ErlTyper[ErlangParser.TokStringContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokStringContext): Result[ErlType] = Success(env, ErlString)
  }

  implicit object TokVar extends ErlTyper[ErlangParser.TokVarContext] {
    def check(env: TypeEnv, ctx: ErlangParser.TokVarContext): Result[ErlType] = {
      val name = ctx.getText
      env.get(name).map(Success(env, _)).getOrElse {
        val erlVar = ErlVar(name)
        Success(env.updated(name, erlVar), erlVar)
      }
    }
  }

  implicit object Atomic extends ErlTyper[ErlangParser.AtomicContext] {
    def check(env: TypeEnv, ctx: ErlangParser.AtomicContext): Result[ErlType] =
      ctx.tokChar.check(env) orElse
      ctx.tokInteger.check(env) orElse
      ctx.tokFloat.check(env) orElse
      ctx.tokAtom.check(env) orElse
      Option(ctx.tokString).toResult(env).map(_ => ErlString)
  }

  implicit object ExprMax extends ErlTyper[ErlangParser.ExprMaxContext] {
    def check(env: TypeEnv, ctx: ErlangParser.ExprMaxContext): Result[ErlType] = ctx.tokVar.check(env) orElse ctx.atomic.check(env)
  }

  implicit object Expr800 extends ErlTyper[ErlangParser.Expr800Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr800Context): Result[ErlType] = ctx.exprMax.get(0).check(env)
  }

  implicit object FunctionCall extends ErlTyper[ErlangParser.FunctionCallContext] {
    def check(env: TypeEnv, ctx: ErlangParser.FunctionCallContext): Result[ErlType] =
      ctx.expr800.check(env).flatMap { (env, f) =>
        checkAll(env, ctx.argumentList.exprs).flatMap(application(_, f, _))
      }
  }

  implicit object Expr700 extends ErlTyper[ErlangParser.Expr700Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr700Context): Result[ErlType] = ctx.expr800.check(env) orElse ctx.functionCall.check(env)
  }

  implicit object Expr600 extends ErlTyper[ErlangParser.Expr600Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr600Context): Result[ErlType] = ctx.expr700.check(env)
  }

  implicit object Expr500 extends ErlTyper[ErlangParser.Expr500Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr500Context): Result[ErlType] = operations(env, ctx.expr600.asScala, Option(ctx.multOp).map(_.asScala.map(_.getText)))
  }

  implicit object Expr400 extends ErlTyper[ErlangParser.Expr400Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr400Context): Result[ErlType] = operations(env, ctx.expr500.asScala, Option(ctx.addOp).map(_.asScala.map(_.getText)))
  }

  implicit object Expr300 extends ErlTyper[ErlangParser.Expr300Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr300Context): Result[ErlType] = operations(env, ctx.expr400.asScala, Option(ctx.listOp).map(_.asScala.map(_.getText)))
  }

  implicit object Expr200 extends ErlTyper[ErlangParser.Expr200Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr200Context): Result[ErlType] = operation(env, ctx.expr300.asScala, Option(ctx.compOp).map(_.getText))
  }

  implicit object Expr160 extends ErlTyper[ErlangParser.Expr160Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr160Context): Result[ErlType] = operation(env, ctx.expr200.asScala, Some("andalso"))
  }

  implicit object Expr150 extends ErlTyper[ErlangParser.Expr150Context] {
    def check(env: TypeEnv, ctx: ErlangParser.Expr150Context): Result[ErlType] = operation(env, ctx.expr160.asScala, Some("orelse"))
  }

  implicit object Expr extends ErlTyper[ErlangParser.ExprContext] {
    def check(env: TypeEnv, ctx: ErlangParser.ExprContext): Result[ErlType] = ctx.expr100.expr150.get(0).check(env)
  }

  implicit object FunctionClause extends ErlTyper[ErlangParser.FunctionClauseContext] {
    def check(env: TypeEnv, ctx: ErlangParser.FunctionClauseContext): Result[ErlType] =
      checkAll(env, ctx.clauseArgs.argumentList.exprs).flatMap { (env, args) =>
        checkAll(env, ctx.clauseBody.exprs).map { body =>
          ErlFunction(args, body.last)
        }
      }
  }

  def checkAll(env: TypeEnv, ctx: ErlangParser.ExprsContext): Result[List[ErlType]] = {
    val exprs = ctx.expr.asScala
    exprs.foldRight(Success(env, Nil): Result[List[ErlType]]) {
      case (expr, result) =>
        result.flatMap((env, types) => expr.check(env).map(_ :: types))
    }
  }

  def application(env: TypeEnv, f: ErlType, args: List[ErlType]): Result[ErlType] = {
    f match {
      case ErlAtom(name) => env.get(name).map(application(env, _, args)).getOrElse(Result.NotFound(name))
      case ErlFunction(params, body) => unifyAll(env, params, args).map(_ => body)
      case _ => throw new Exception(s"$f is not function")
    }
  }

  def operation[A: ErlTyper](env: TypeEnv, exprs: Seq[A], op: Option[String]): Result[ErlType] = {
    val lhs = exprs.head.check(env)
    exprs.tail.headOption.fold(lhs) { rhs =>
        lhs.flatMap { (env, lhs) =>
          rhs.check(env).flatMap { (env, rhs) =>
            env.get(op.get).map(application(env, _, List(lhs, rhs))).getOrElse(Result.NotFound(op.get))
          }
        }
    }
  }

  def operations[A: ErlTyper](env: TypeEnv, exprs: Seq[A], ops: Option[Seq[String]]): Result[ErlType] = {
    val lhs = exprs.head.check(env)
    ops.fold(lhs) { ops =>
      (ops zip exprs.tail).foldLeft(lhs) {
        case (result, (op, expr)) =>
          result.flatMap { (env, lhs) =>
            expr.check(env).flatMap { (env, rhs) =>
              env.get(op).map(application(env, _, List(lhs, rhs))).getOrElse(Result.NotFound(op))
            }
          }
      }
    }
  }

}
