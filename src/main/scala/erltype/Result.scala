package erltype

sealed abstract class Result[+A] {

  final def orElse[B >: A](that: => Result[B]): Result[B] =
    this match {
      case Success(env, typ) => Success(env, typ)
      case Failure(messages) => Failure(messages)
      case Null => that
    }

  final def flatMap[B](f: (TypeEnv, A) => Result[B]): Result[B] =
    this match {
      case Success(env, typ) => f(env, typ)
      case Failure(message) => Failure(message)
      case Null => Null
    }

  final def map[B](f: A => B): Result[B] = flatMap((env, typ) => Success(env, f(typ)))

  final def isSuccess: Boolean =
    this match {
      case Success(_, _) => true
      case _ => false
    }

}

case class Success[A](env: TypeEnv, typ: A) extends Result[A]

case class Failure(message: String) extends Result[Nothing]

case object Null extends Result[Nothing]

object Result {
  def NotFound(name: String): Result[Nothing] = Failure(s"$name is not found")
  def TypeMismatch(param: ErlType, arg: ErlType): Result[Nothing] = Failure(s"expected: $param; actual: $arg")
}
