package erltype

sealed abstract class ErlType

case object ErlInteger extends ErlType

case object ErlFloat extends ErlType

case object ErlChar extends ErlType

case object ErlString extends ErlType

case class ErlAtom(name: String) extends ErlType

case class ErlFunction(args: List[ErlType], body: ErlType) extends ErlType

case class ErlVar(name: String) extends ErlType
