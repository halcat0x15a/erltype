package object erltype {

  type TypeEnv = Map[String, ErlType]

  implicit class Check[A](val a: A) extends AnyVal {
    def check(env: TypeEnv)(implicit A: ErlTyper[A]): Result[ErlType] = {
      Option(a).toResult(env).flatMap(A.check)
    }
  }

  implicit class OptionToResult[A](val option: Option[A]) extends AnyVal {
    def toResult(env: TypeEnv): Result[A] = option.fold(Null: Result[A])(Success(env, _))
  }

}
