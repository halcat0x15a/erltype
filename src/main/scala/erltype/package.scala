package object erltype {

  type Delta = Map[String, ErlType[Minus]]

  type Pi = Map[String, TypingScheme[ErlType[Plus]]]

  implicit class Check[A](val ctx: A) extends AnyVal {
    def check_+(env: Pi)(implicit A: ErlTyper[A]) = A.check_+(env, ctx)
    def check_-(env: Delta)(implicit A: ErlTyper[A]) = A.check_-(env, ctx)
  }

}
