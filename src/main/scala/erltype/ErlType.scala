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

sealed abstract class ErlType[A <: Polarity] {
  val polarity: A
}

case class ErlInteger[A <: Polarity]()(implicit val polarity: A) extends ErlType[A] {
  override def toString = "integer"
}

case class ErlFloat[A <: Polarity]()(implicit val polarity: A) extends ErlType[A] {
  override def toString = "float"
}

case class ErlChar[A <: Polarity]()(implicit val polarity: A) extends ErlType[A] {
  override def toString = "char"
}

case class ErlString[A <: Polarity]()(implicit val polarity: A) extends ErlType[A] {
  override def toString = "string"
}

case class ErlList[A <: Polarity](typ: ErlType[A])(implicit val polarity: A) extends ErlType[A] {
  override def toString = s"[$typ]"
}

case class ErlAtom[A <: Polarity](name: String)(implicit val polarity: A) extends ErlType[A] {
  override def toString = s""""$name""""
}

case class ErlUnion(types: List[ErlType[Plus]]) extends ErlType[Plus] {
  val polarity = Polarity.Plus
  override def toString = types.size match {
    case 0 => "bottom"
    case 1 => types.head.toString
    case _ => types.mkString("(", " | ", ")")
  }
}

case class ErlIntersection(types: List[ErlType[Minus]]) extends ErlType[Minus] {
  val polarity = Polarity.Minus
  override def toString = types.size match {
    case 0 => "top"
    case 1 => types.head.toString
    case _ => types.mkString("(", " & ", ")")
  }
}

case class ErlFunction[A <: Polarity](params: List[ErlType[A#Inverse]], ret: ErlType[A])(implicit val polarity: A) extends ErlType[A] {
  override def toString = {
    val ps = params.mkString("(", ", ", ")")
    s"($ps -> $ret)"
  }
}

case class ErlVar[A <: Polarity](polarity: A, id: Long) extends ErlType[A] {
  override def toString = s"$polarity$id"
}

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

  def fresh[A <: Polarity](implicit A: A): ErlVar[A] = ErlVar(A, idGen.getAndIncrement)

  def isFreeVar[A <: Polarity](id: Long, typ: ErlType[A]): Boolean =
    typ match {
      case ErlList(x) => isFreeVar(id, x)
      case ErlFunction(xs, x) => xs.exists(isFreeVar(id, _)) || isFreeVar(id, x)
      case ErlUnion(xs) => xs.exists(isFreeVar(id, _))
      case ErlIntersection(xs) => xs.exists(isFreeVar(id, _))
      case ErlVar(_, x) => x == id
      case _ => true
    }

  def bisubst[A <: Polarity](typ: ErlType[A], subst: Map[ErlVar[_ <: Polarity], ErlType[_ <: Polarity]])(implicit A: A): ErlType[A] =
    typ match {
      case ErlList(typ) => ErlList(bisubst(typ, subst))
      case ErlFunction(params, ret) => ErlFunction(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case ErlUnion(types) => types.map(bisubst(_, subst)).foldLeft(Bottom)(_ \/ _)
      case ErlIntersection(types) => types.map(bisubst(_, subst)).foldLeft(Top)(_ /\ _)
      case v@ErlVar(_, _) if subst.contains(v) => subst(v).asInstanceOf[ErlType[A]]
      case _ => typ
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: ErlType[A], y: ErlType[B])(implicit A: A, B: B): ErlType[A] =
    x match {
      case ErlList(x) => ErlList(bisubst(id, x, y))
      case ErlFunction(xs, x) => ErlFunction(xs.map(bisubst(id, _, y)(A.inverse, B)), bisubst(id, x, y))
      case ErlUnion(xs) => xs.map(bisubst(id, _, y)).foldLeft(Bottom)(_ \/ _)
      case ErlIntersection(xs) => xs.map(bisubst(id, _, y)).foldLeft(Top)(_ /\ _)
      case ErlVar(p, x) if x == id && A == B => y.asInstanceOf[ErlType[A]]
      case _ => x
    }

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
      case (ErlVar(_, id), _) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_-(id, y /\ ErlVar(Polarity.Minus, id)))
        else
          BiSubsts(BiSubst_-(id, bisubst(id, y, fresh[Minus]) /\ ErlVar(Polarity.Minus, id)))
      case (_, ErlVar(_, id)) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_+(id, x \/ ErlVar(Polarity.Plus, id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, x, fresh[Plus]) \/ ErlVar(Polarity.Plus, id)))
      case _ =>
        throw new RuntimeException(s"$x is not $y")
    }

  def collect[A <: Polarity](typ: ErlType[A])(implicit A: A): Vector[ErlVar[_ <: Polarity]] =
    typ match {
      case ErlList(x) => collect(x)
      case ErlFunction(xs, x) => xs.foldLeft(Vector.empty[ErlVar[_ <: Polarity]])(_ ++ collect(_)(A.inverse)) ++ collect(x)
      case ErlUnion(xs) => xs.foldLeft(Vector.empty[ErlVar[_ <: Polarity]])(_ ++ collect(_))
      case ErlIntersection(xs) => xs.foldLeft(Vector.empty[ErlVar[_ <: Polarity]])(_ ++ collect(_))
      case v@ErlVar(_, _) => Vector(v)
      case _ => Vector.empty
    }

  def simplify[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ).groupBy(_.polarity).mapValues(_.map(_.id).toSet)
    val ps = vars.getOrElse(Polarity.Plus, Set.empty)
    val ms = vars.getOrElse(Polarity.Minus, Set.empty)
    val free = (ps | ms) -- (ps & ms)
    reassign(free.foldLeft(typ)((typ, id) => bisubst(id, bisubst(id, typ, Top), Bottom)))
  }

  def reassign[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ)
    val index = vars.map(_.id).distinct.zipWithIndex.toMap
    println(vars)
    bisubst(typ, vars.map { case v => v -> ErlVar(v.polarity, index(v.id)) }.toMap)
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
