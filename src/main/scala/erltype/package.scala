package object erltype {

  type Delta = Map[String, ErlType[Minus]]

  type Pi = Map[String, TypingScheme[ErlType[Plus]]]

}
