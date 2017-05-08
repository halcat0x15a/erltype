import java.util.concurrent.atomic.AtomicLong

package object erltype {

  type Delta = Map[Long, Type[Neg]]

  type Pi = Map[String, TypingScheme[Type[Pos]]]

  def TopType[A <: Polarity]: Type[A] = IntersectionType(Vector.empty)

  def BottomType[A <: Polarity]: Type[A] = UnionType(Vector.empty)

  def BooleanType[A <: Polarity]: Type[A] = AtomType("true")

  private val idGen: AtomicLong = new AtomicLong(0)

  def fresh: Long = idGen.getAndIncrement

}
