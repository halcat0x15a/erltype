package object erltype {

  type Delta = Map[Long, ErlType[Minus]]

  type Pi = Map[String, TypingScheme[ErlType[Plus]]]

  def UnitType[A <: Polarity]: TupleType[A] = TupleType(Nil)

  def ListType[A <: Polarity](typ: ErlType[A]): ErlType[A] = {
    val v = ErlType.fresh
    RecursiveType(v, VariantType(Map("nil" -> UnitType, "cons" -> TupleType(List(typ, VarType(v))))))
  }

}
