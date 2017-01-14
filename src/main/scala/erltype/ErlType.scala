package erltype

sealed abstract class ErlType {

  final def display: String =
    this match {
      case ErlInteger => "integer"
      case ErlFloat => "float"
      case ErlChar => "char"
      case ErlString => "string"
      case ErlAtom(name) => s"'$name'"
      case ErlFunction(args, body) => s"""${args.map(_.display).mkString("(", ", ", ")")} -> ${body.display}"""
      case ErlVar(name) => name
      case ErlUnion(types) => types.map(_.display).mkString(" | ")
      case ErlList(typ) => s"[${typ.display}]"
    }

}

case object ErlInteger extends ErlType

case object ErlFloat extends ErlType

case object ErlChar extends ErlType

case object ErlString extends ErlType

case class ErlAtom(name: String) extends ErlType

case class ErlFunction(args: List[ErlType], body: ErlType) extends ErlType

case class ErlVar(name: String) extends ErlType

case class ErlUnion(types: List[ErlType]) extends ErlType

case class ErlList(typ: ErlType) extends ErlType

object ErlType {
  val Boolean: ErlType = ErlUnion(List(ErlAtom("true"), ErlAtom("false")))
  val Number: ErlType = ErlUnion(List(ErlInteger, ErlFloat))
}
