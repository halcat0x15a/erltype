package object erltype {

  implicit class Check[A](val ctx: A) extends AnyVal {
    def check(env: Map[String, TypingScheme[ErlType[Plus]]])(implicit A: ErlTyper[A]): TypingScheme[ErlType[Plus]] = A.check(env, ctx)
  }

}
