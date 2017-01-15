package erltype

import scala.collection.JavaConverters._

trait ErlTyper[A] {
  def check(env: TypeEnv, ctx: A): Result[ErlType]
}

object ErlTyper {

  def union(a: ErlType, b: ErlType): ErlType = {
    (a, b) match {
      case (ErlFunction(params, ret), ErlFunction(args, body)) =>
        ErlFunction((params zip args).map { case (param, arg) => union(param, arg) }, union(ret, body))
      case (ErlVar(_), _) => b
      case (_, ErlVar(_)) => a
      case (ErlList(a), ErlList(b)) => ErlList(union(a, b))
      case (ErlUnion(xs), ErlUnion(ys)) => ErlUnion((xs ::: ys).distinct)
      case (ErlUnion(xs), _) => ErlUnion((b :: xs).distinct)
      case (_, ErlUnion(xs)) => ErlUnion((a :: xs).distinct)
      case _ if a == b => a
      case _ => ErlUnion(List(a, b))
    }
  }

  def resolve(env: TypeEnv, typ: ErlType): ErlType =
    typ match {
      case ErlFunction(args, body) => ErlFunction(args.map(resolve(env, _)), resolve(env, body))
      case ErlUnion(types) => ErlUnion(types.map(resolve(env, _)))
      case ErlVar(name) => env.get(name).map(v => if (v == typ) typ else resolve(env, v)).getOrElse(typ)
      case ErlList(typ) => ErlList(resolve(env, typ))
      case _ => typ
    }

  def unify(env: TypeEnv, param: ErlType, arg: ErlType): Result[ErlType] =
    (param, arg) match {
      case (ErlVar(x), ErlVar(y)) if x == y => Success(env, param)
      case (ErlVar(name), _) => Success(env.updated(name, arg), arg)
      //env.get(name).fold(Success(env.updated(name, arg), arg): Result[ErlType])(param => unify(env, resolve(env, param), arg))
      case (_, ErlVar(name)) => Success(env.updated(name, param), param)
      //env.get(name).fold(Success(env.updated(name, param), param): Result[ErlType])(arg => unify(env, param, resolve(env, arg)))
      case (ErlList(a), ErlList(b)) => unify(env, a, b).map(ErlList(_))
      case (ErlFunction(params, ret), ErlFunction(args, body)) =>
        unifyAll(env, params, args).flatMap { (env, types) =>
          unify(env, ret, body).map(ErlFunction(types, _))
        }
      case (ErlUnion(params), ErlUnion(args)) => subsetOf(env, params, args).map(ErlUnion(_))
      case (ErlUnion(types), _) => types.map(unify(env, _, arg)).find(_.isSuccess).getOrElse(Result.TypeMismatch(param, arg))
      case _  if param == arg => Success(env, param)
      case _ => Result.TypeMismatch(param, arg)
    }

  def unifyAll(env: TypeEnv, params: List[ErlType], args: List[ErlType]): Result[List[ErlType]] =
    (params zip args).foldRight(Success(env, Nil): Result[List[ErlType]]) {
      case ((param, arg), result) =>
        result.flatMap { (env, types) =>
          unify(env, param, arg).map(_ :: types)
        }
    }

  def subsetOf(env: TypeEnv, supers: List[ErlType], subs: List[ErlType]): Result[List[ErlType]] =
    subs.foldRight(Success(env, Nil): Result[List[ErlType]]) { (sub, result) =>
      result.flatMap { (env, types) =>
        supers.map(unify(env, _, sub)).find(_.isSuccess).getOrElse(Result.TypeMismatch(ErlUnion(supers), sub)).map(_ :: types)
      }
    }

  def checkAll[A: ErlTyper](env: TypeEnv, exprs: Seq[A]): Result[List[ErlType]] = {
    exprs.foldRight(Success(env, Nil): Result[List[ErlType]]) { (expr, result) =>
      result.flatMap((env, types) => expr.check(env).map(_ :: types))
    }
  }

  def define(env: TypeEnv, name: String, typ: ErlType): Result[ErlType] =
    env.get(name).fold(Success(env.updated(name, typ), typ): Result[ErlType])(unify(env, _, typ))

  def application(env: TypeEnv, f: ErlType, args: List[ErlType]): Result[ErlType] = {
    f match {
      case ErlVar(name) => unify(env, f, ErlFunction(args, ErlVar("AppReturnType"))).flatMap(application(_, _, args))
      case ErlAtom(name) => env.get(name).map(application(env, _, args)).getOrElse(Result.NotFound(name))
      case ErlFunction(params, body) => unifyAll(env, params, args).map(_ => body)
      case _ => Failure(s"$f is not function")
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

  import typer._

  implicit object TokChar extends TokChar

  implicit object TokInteger extends TokInteger

  implicit object TokFloat extends TokFloat

  implicit object TokAtom extends TokAtom

  implicit object TokString extends TokString

  implicit object TokVar extends TokVar

  implicit object Atomic extends Atomic

  implicit object ExprMaxContext extends ExprMaxContext

  implicit object Expr800Context extends Expr800Context

  implicit object Expr700Context extends Expr700Context

  implicit object Expr600Context extends Expr600Context

  implicit object Expr500Context extends Expr500Context

  implicit object Expr400Context extends Expr400Context

  implicit object Expr300Context extends Expr300Context

  implicit object Expr200Context extends Expr200Context

  implicit object Expr160Context extends Expr160Context

  implicit object Expr150Context extends Expr150Context

  implicit object ExprContext extends ExprContext

  implicit object FunctionCallContext extends FunctionCallContext

  implicit object ListContext extends ListContext

  implicit object ListComprehensionContext extends ListComprehensionContext

  implicit object FunctionClause extends FunctionClause

}
