package erltype

import java.util.concurrent.atomic.AtomicLong

sealed abstract class Polarity {
  type Inverse <: Polarity
  def inverse: Inverse
}

sealed abstract class Plus extends Polarity {
  type Inverse = Minus
  def inverse = Polarity.Minus
  override def toString = "+"
}

sealed abstract class Minus extends Polarity {
  type Inverse = Plus
  def inverse = Polarity.Plus
  override def toString = "-"
}

object Polarity {
  implicit case object Plus extends Plus
  implicit case object Minus extends Minus
}

sealed abstract class ErlType[A <: Polarity]

case class ErlInteger[A <: Polarity]() extends ErlType[A] {
  override def toString = "integer"
}

case class ErlFloat[A <: Polarity]() extends ErlType[A] {
  override def toString = "float"
}

case class ErlChar[A <: Polarity]() extends ErlType[A] {
  override def toString = "char"
}

case class ErlString[A <: Polarity]() extends ErlType[A] {
  override def toString = "string"
}

case class ErlList[A <: Polarity](typ: ErlType[A]) extends ErlType[A] {
  override def toString = s"[$typ]"
}

case class ErlAtom[A <: Polarity](name: String) extends ErlType[A] {
  override def toString = s""""$name""""
}

case class ErlUnion(types: List[ErlType[Plus]]) extends ErlType[Plus] {
  override def toString = types match {
    case Nil => "bottom"
    case typ :: Nil => typ.toString
    case _ => types.mkString("(", " | ", ")")
  }
}

case class ErlIntersection(types: List[ErlType[Minus]]) extends ErlType[Minus] {
  override def toString = types match {
    case Nil => "top"
    case typ :: Nil => typ.toString
    case _ => types.mkString("(", " & ", ")")
  }
}

case class ErlFunction[A <: Polarity](params: List[ErlType[A#Inverse]], ret: ErlType[A]) extends ErlType[A] {
  override def toString = {
    val ps = params.mkString("(", ", ", ")")
    s"($ps -> $ret)"
  }
}

case class ErlVar[A <: Polarity](id: Long) extends ErlType[A] {
  override def toString = (id + 'A').toChar.toString
}

case class TypeVar(id: Long, polarity: Polarity)

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

  val Top: ErlType[Minus] = ErlIntersection(Nil)

  val Bottom: ErlType[Plus] = ErlUnion(Nil)

  private val idGen: AtomicLong = new AtomicLong

  def fresh[A <: Polarity](implicit A: A): ErlVar[A] = ErlVar(idGen.getAndIncrement)

  def collect[A <: Polarity](typ: ErlType[A])(implicit A: A): Vector[TypeVar] =
    typ match {
      case ErlList(typ) =>
        collect(typ)
      case ErlFunction(params, ret) =>
        params.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_)(A.inverse)) ++ collect(ret)
      case ErlUnion(types) =>
        types.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_))
      case ErlIntersection(types) =>
        types.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_))
      case ErlVar(id) =>
        Vector(TypeVar(id, A))
      case _ =>
        Vector.empty
    }

  def isFreeVar[A <: Polarity](id: Long, typ: ErlType[A])(implicit A: A): Boolean = collect(typ).exists(_.id == id)

  def bisubst[A <: Polarity](typ: ErlType[A], subst: Map[TypeVar, ErlType[_ <: Polarity]])(implicit A: A): ErlType[A] =
    typ match {
      case ErlList(typ) =>
        ErlList(bisubst(typ, subst))
      case ErlFunction(params, ret) =>
        ErlFunction(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case ErlUnion(types) =>
        types.map(bisubst(_, subst)).foldLeft(Bottom)(_ \/ _)
      case ErlIntersection(types) =>
        types.map(bisubst(_, subst)).foldLeft(Top)(_ /\ _)
      case ErlVar(id) =>
        subst.get(TypeVar(id, A)).fold(typ)(_.asInstanceOf[ErlType[A]])
      case _ =>
        typ
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: ErlType[A], y: ErlType[B])(implicit A: A, B: B): ErlType[A] = bisubst(x, Map(TypeVar(id, B) -> y))

  def biunify(x: ErlType[Plus], y: ErlType[Minus]): BiSubsts =
    (x, y) match {
      case _ if x == y =>
        BiSubsts.identity
      case (ErlList(x), ErlList(y)) =>
        biunify(x, y)
      case (ErlFunction(xs, x), ErlFunction(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (ErlUnion(xs), _) =>
        xs.foldRight(BiSubsts.identity) { (x, f) => f compose biunify(f(x), f(y)) }
      case (_, ErlIntersection(ys)) =>
        ys.foldRight(BiSubsts.identity) { (y, f) => f compose biunify(f(x), f(y)) }
      case (ErlVar(id), _) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_-(id, y /\ ErlVar(id)))
        else
          BiSubsts(BiSubst_-(id, bisubst(id, y, fresh[Minus]) /\ ErlVar(id)))
      case (_, ErlVar(id)) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_+(id, x \/ ErlVar(id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, x, fresh[Plus]) \/ ErlVar(id)))
      case _ =>
        throw new RuntimeException(s"$x is not $y")
    }

  def simplify[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ).groupBy(_.polarity).mapValues(_.map(_.id).toSet)
    val ps = vars.getOrElse(Polarity.Plus, Set.empty)
    val ms = vars.getOrElse(Polarity.Minus, Set.empty)
    val free = (ps | ms) -- (ps & ms)
    reassign(bisubst(typ, free.flatMap(id => List(TypeVar(id, Polarity.Plus) -> Bottom, TypeVar(id, Polarity.Minus) -> Top))(collection.breakOut)))
  }

  def reassign[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ)
    val index = vars.map(_.id).distinct.zipWithIndex.toMap
    bisubst(typ, vars.map { case v => v -> ErlVar(index(v.id)) }(collection.breakOut))
  }

  implicit class PlusOp(val self: ErlType[Plus]) extends AnyVal {
    def \/(that: ErlType[Plus]): ErlType[Plus] =
      (self, that) match {
        case _ if self == that => self
        case (ErlUnion(xs), ErlUnion(ys)) => ErlUnion(xs ::: ys)
        case (ErlUnion(xs), _) => xs.foldRight(that)(_ \/ _)
        case (_, ErlUnion(ys)) => ErlUnion(self :: ys)
        case (ErlList(x), ErlList(y)) => ErlList(x \/ y)
        case (ErlFunction(xs, x), ErlFunction(ys, y)) => ErlFunction(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
        case _ => ErlUnion(List(self, that))
      }
  }

  implicit class MinusOp(val self: ErlType[Minus]) extends AnyVal {
    def /\(that: ErlType[Minus]): ErlType[Minus] =
      (self, that) match {
        case _ if self == that => self
        case (ErlIntersection(xs), ErlIntersection(ys)) => ErlIntersection(xs ::: ys)
        case (ErlIntersection(xs), _) => xs.foldRight(that)(_ /\ _)
        case (_, ErlIntersection(ys)) => ErlIntersection(self :: ys)
        case (ErlList(x), ErlList(y)) => ErlList(x /\ y)
        case (ErlFunction(xs, x), ErlFunction(ys, y)) => ErlFunction(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
        case _ => ErlIntersection(List(self, that))
      }
  }

}
