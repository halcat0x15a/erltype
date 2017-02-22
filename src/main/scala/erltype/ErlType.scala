package erltype

import java.util.concurrent.atomic.AtomicLong

sealed abstract class Polarity {
  type Inverse <: Polarity
  def inverse: Inverse
}

sealed abstract class Plus extends Polarity {
  type Inverse = Minus
  val inverse = Polarity.Minus
}

sealed abstract class Minus extends Polarity {
  type Inverse = Plus
  val inverse = Polarity.Plus
}

object Polarity {
  implicit case object Plus extends Plus
  implicit case object Minus extends Minus
}

sealed abstract class ErlType[A <: Polarity](val show: String)

case class ErlInteger[A <: Polarity]() extends ErlType[A]("integer")

case class ErlFloat[A <: Polarity]() extends ErlType[A]("float")

case class ErlChar[A <: Polarity]() extends ErlType[A]("char")

case class ErlString[A <: Polarity]() extends ErlType[A]("string")

case class ErlAtom[A <: Polarity](name: String) extends ErlType[A]("\"name\"")

case class ErlUnion(types: List[ErlType[Plus]]) extends ErlType[Plus](types.map(_.show).mkString("(", " | ", ")"))

case class ErlIntersection(types: List[ErlType[Minus]]) extends ErlType[Minus](types.map(_.show).mkString("(", " & ", ")"))

case class ErlFunction[A <: Polarity](params: List[ErlType[A#Inverse]], typ: ErlType[A]) extends ErlType[A](s"""((${params.map(_.show).mkString(", ")}) -> ${typ.show})""")

case class ErlVar[A <: Polarity](id: Long) extends ErlType[A](id.toString)

sealed abstract class BiSubst

case class BiSubst_+(id: Long, typ: ErlType[Plus]) extends BiSubst

case class BiSubst_-(id: Long, typ: ErlType[Minus]) extends BiSubst

class BiSubsts(val self: Vector[BiSubst]) extends AnyVal {
  def apply[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] =
    self.foldLeft(typ) { (typ, bisubst) =>
      bisubst match {
        case BiSubst_+(id, plus) => ErlType.bisubst(id, typ, plus)
        case BiSubst_-(id, minus) => ErlType.bisubst(id, typ, minus)
      }
    }
  def compose(that: BiSubsts): BiSubsts = new BiSubsts(self ++ that.self)
}

object BiSubsts {
  val identity: BiSubsts = new BiSubsts(Vector.empty)
  def apply(bisubst: BiSubst): BiSubsts = new BiSubsts(Vector(bisubst))
}

object ErlType {

  private val idGen: AtomicLong = new AtomicLong

  def fresh[A <: Polarity]: ErlVar[A] = ErlVar(idGen.getAndIncrement)

  def isFreeVar[A <: Polarity](id: Long, typ: ErlType[A]): Boolean =
    typ match {
      case ErlFunction(xs, x) => xs.exists(isFreeVar(id, _)) || isFreeVar(id, x)
      case ErlUnion(xs) => xs.exists(isFreeVar(id, _))
      case ErlIntersection(xs) => xs.exists(isFreeVar(id, _))
      case ErlVar(x) => x == id
      case _ => true
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: ErlType[A], y: ErlType[B])(implicit A: A, B: B): ErlType[A] =
    x match {
      case ErlFunction(xs, x) => ErlFunction(xs.map(bisubst(id, _, y)(A.inverse, B)), bisubst(id, x, y))
      case ErlUnion(xs) => ErlUnion(xs.map(bisubst(id, _, y)))
      case ErlIntersection(xs) => ErlIntersection(xs.map(bisubst(id, _, y)))
      case ErlVar(x) if x == id && A == B => y.asInstanceOf[ErlType[A]]
      case _ => x
    }

  def biunify(x: ErlType[Plus], y: ErlType[Minus]): BiSubsts =
    (x, y) match {
      case _ if x == y =>
        BiSubsts.identity
      case (ErlFunction(xs, x), ErlFunction(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (ErlUnion(xs), _) =>
        xs.foldRight(BiSubsts.identity) { (x, f) => f compose biunify(f(x), f(y)) }
      case (_, ErlIntersection(ys)) =>
        ys.foldRight(BiSubsts.identity) { (y, f) => f compose biunify(f(x), f(y)) }
      case (ErlVar(id), _) =>
        if (isFreeVar(id, y)) {
          BiSubsts(BiSubst_-(id, y /\ ErlVar(id)))
        } else {
          BiSubsts(BiSubst_-(id, bisubst(id, y, fresh[Minus]) /\ ErlVar(id)))
        }
      case (_, ErlVar(id)) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_+(id, x \/ ErlVar(id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, x, fresh[Plus]) \/ ErlVar(id)))
      case _ =>
        throw new RuntimeException
    }

  implicit class PlusOp(val self: ErlType[Plus]) extends AnyVal {
    def \/(that: ErlType[Plus]): ErlType[Plus] =
      (self, that) match {
        case (ErlUnion(xs), ErlUnion(ys)) => ErlUnion(xs ::: ys)
        case (ErlUnion(Nil), _) => that
        case (_, ErlUnion(Nil)) => self
        case (ErlFunction(xs, x), ErlFunction(ys, y)) => ErlFunction(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
        case _ => ErlUnion(List(self, that))
      }
  }

  implicit class MinusOp(val self: ErlType[Minus]) extends AnyVal {
    def /\(that: ErlType[Minus]): ErlType[Minus] =
      (self, that) match {
        case (ErlIntersection(xs), ErlIntersection(ys)) => ErlIntersection(xs ::: ys)
        case (ErlIntersection(Nil), _) => that
        case (_, ErlIntersection(Nil)) => self
        case (ErlFunction(xs, x), ErlFunction(ys, y)) => ErlFunction(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
        case _ => ErlIntersection(List(self, that))
      }
  }

}
