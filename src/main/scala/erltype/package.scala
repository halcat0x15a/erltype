import java.util.concurrent.atomic.AtomicLong

package object erltype {

  type Delta = Map[Long, Type[Neg]]

  type Pi = Map[String, TypingScheme[Type[Pos]]]

  type Vars = Vector[Long]

  val TopType: Type[Neg] = IntersectionType(Vector.empty)

  val BottomType: Type[Pos] = UnionType(Vector.empty)

  def sum(types: Seq[Type[Pos]]): Type[Pos] = types.foldLeft(BottomType)(_ \/ _)

  def prod(types: Seq[Type[Neg]]): Type[Neg] = types.foldLeft(TopType)(_ /\ _)

  def BooleanType[A <: Polarity]: Type[A] = AtomType("true")

  private val idGen: AtomicLong = new AtomicLong(0)

  def fresh: Long = idGen.getAndIncrement

  def alphabets(n: Long): String = (if (n / 26 > 0) alphabets(n / 26 - 1) else "") + ('A' + n % 26).toChar

}
