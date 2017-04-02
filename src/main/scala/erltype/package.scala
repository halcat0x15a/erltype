package object erltype {

  type Delta = Map[Long, ErlType[Minus]]

  type Pi = Map[String, TypingScheme[ErlType[Plus]]]

}
