import java.util.concurrent.atomic.AtomicLong

package object erltype {

  type Delta = Map[Long, Type[Neg]]

  type Pi = Map[String, TypingScheme[Type[Pos]]]

  val TopType: Type[Neg] = IntersectionType(Vector.empty)

  val BottomType: Type[Pos] = UnionType(Vector.empty)

  private val idGen: AtomicLong = new AtomicLong(0)

  def fresh: Long = idGen.getAndIncrement

}
