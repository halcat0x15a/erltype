package erltype

import java.util.concurrent.atomic.AtomicLong

sealed abstract class Polarity {
  type Inverse <: Polarity
  def inverse: Inverse
}

sealed abstract class Plus extends Polarity {
  type Inverse = Minus
  def inverse = Polarity.Minus
}

sealed abstract class Minus extends Polarity {
  type Inverse = Plus
  def inverse = Polarity.Plus
}

object Polarity {
  implicit case object Plus extends Plus
  implicit case object Minus extends Minus
}

sealed abstract class ErlType[A <: Polarity] {
  def show: String
}

case class IntType[A <: Polarity]() extends ErlType[A] {
  def show = "integer"
}

case class FloatType[A <: Polarity]() extends ErlType[A] {
  def show = "float"
}

case class CharType[A <: Polarity]() extends ErlType[A] {
  def show = "char"
}

case class StringType[A <: Polarity]() extends ErlType[A] {
  def show = "string"
}

case class AtomType[A <: Polarity](name: String) extends ErlType[A] {
  def show = s""""$name""""
}

case class RecursiveType[A <: Polarity](id: Long, typ: ErlType[A]) extends ErlType[A] {
  def show = s"rec ${VarType(id).show} = ${typ.show}"
}

case class TupleType[A <: Polarity](types: List[ErlType[A]]) extends ErlType[A] {
  def show = types.map(_.show).mkString("(", ", ", ")")
}

case class VariantType[A <: Polarity](types: Map[String, ErlType[A]]) extends ErlType[A] {
  def show = types.map { case (label, typ) => s"$label: ${typ.show}" }.mkString("[", " | ", "]")
}

case class UnionType(types: Vector[ErlType[Plus]]) extends ErlType[Plus] {
  def show = if (types.isEmpty) "bottom" else types.map(_.show).mkString(" | ")
}

case class IntersectionType(types: Vector[ErlType[Minus]]) extends ErlType[Minus] {
  def show = if (types.isEmpty) "top" else types.map(_.show).mkString(" & ")
}

case class FunctionType[A <: Polarity](params: List[ErlType[A#Inverse]], ret: ErlType[A]) extends ErlType[A] {
  def show = {
    val ps = params.map(_.show).mkString("(", ", ", ")")
    s"($ps -> ${ret.show})"
  }
}

case class VarType[A <: Polarity](id: Long) extends ErlType[A] {
  def show = alphabets(id)
  private[this] def alphabets(n: Long): String = (if (n / 26 > 0) alphabets(n / 26 - 1) else "") + ('A' + n % 26).toChar
}

case class TypeVar(id: Long, polarity: Polarity)

sealed abstract class BiSubst {
  def show = {
    val (id, typ) = this match {
      case BiSubst_+(id, typ) => (id, typ)
      case BiSubst_-(id, typ) => (id, typ)
    }
    s"${VarType(id).show} = ${typ.show}"
  }
}

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

  val Top: ErlType[Minus] = IntersectionType(Vector.empty)

  val Bottom: ErlType[Plus] = UnionType(Vector.empty)

  private val idGen: AtomicLong = new AtomicLong(100)

  def fresh: Long = idGen.getAndIncrement

  def collect[A <: Polarity](typ: ErlType[A])(implicit A: A): Vector[TypeVar] =
    typ match {
      case VariantType(types) =>
        types.foldLeft(Vector.empty[TypeVar])((vars, kv) => vars ++ collect(kv._2))
      case FunctionType(params, ret) =>
        params.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_)(A.inverse)) ++ collect(ret)
      case UnionType(types) =>
        types.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_))
      case IntersectionType(types) =>
        types.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_))
      case TupleType(types) =>
        types.foldLeft(Vector.empty[TypeVar])(_ ++ collect(_))
      case RecursiveType(_, typ) =>
        collect(typ)
      case VarType(id) =>
        Vector(TypeVar(id, A))
      case _ =>
        Vector.empty
    }

  def isFreeVar[A <: Polarity](id: Long, typ: ErlType[A])(implicit A: A): Boolean = collect(typ).exists(_.id == id)

  def bisubst[A <: Polarity](typ: ErlType[A], subst: Map[TypeVar, ErlType[_ <: Polarity]])(implicit A: A): ErlType[A] =
    typ match {
      case VariantType(types) =>
        VariantType(types.map { case (label, typ) => label -> bisubst(typ, subst) })
      case FunctionType(params, ret) =>
        FunctionType(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case UnionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(Bottom)(_ \/ _)
      case IntersectionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(Top)(_ /\ _)
      case VarType(id) =>
        subst.get(TypeVar(id, A)).fold(typ)(_.asInstanceOf[ErlType[A]])
      case RecursiveType(id, typ) =>
        RecursiveType(id, bisubst(typ, subst))
      case _ =>
        typ
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: ErlType[A], y: ErlType[B])(implicit A: A, B: B): ErlType[A] = bisubst(x, Map(TypeVar(id, B) -> y))

  def biunify(x: ErlType[Plus], y: ErlType[Minus]): BiSubsts =
    (x, y) match {
      case _ if x == y =>
        BiSubsts.identity
      case (VariantType(x), VariantType(y)) if x.keySet.subsetOf(y.keySet) =>
        y.keys.foldLeft(BiSubsts.identity) { (f, l) => f compose biunify(f(x(l)), f(y(l))) }
      case (FunctionType(xs, x), FunctionType(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (UnionType(xs), _) =>
        xs.foldLeft(BiSubsts.identity) { (f, x) => f compose biunify(f(x), f(y)) }
      case (_, IntersectionType(ys)) =>
        ys.foldLeft(BiSubsts.identity) { (f, y) => f compose biunify(f(x), f(y)) }
      case (RecursiveType(id, typ), _) =>
        biunify(bisubst(id, typ, x), y)
      case (_, RecursiveType(id, typ)) =>
        biunify(x, bisubst(id, typ, y))
      case (VarType(id), _) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_-(id, y /\ VarType(id)))
        else
          BiSubsts(BiSubst_-(id, bisubst(id, y, VarType[Minus](fresh)) /\ VarType(id)))
      case (_, VarType(id)) =>
        if (isFreeVar(id, x))
          BiSubsts(BiSubst_+(id, x \/ VarType(id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, x, VarType[Plus](fresh)) \/ VarType(id)))
      case (TupleType(xs), TupleType(ys)) if xs.size == ys.size =>
        xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(x), f(y)) }
      case _ =>
        throw new RuntimeException(s"${x.show} is not ${y.show}")
    }

  def removeVar[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ).groupBy(_.polarity).mapValues(_.map(_.id).toSet)
    val ps = vars.getOrElse(Polarity.Plus, Set.empty)
    val ms = vars.getOrElse(Polarity.Minus, Set.empty)
    val free = (ps | ms) -- (ps & ms)
    bisubst(typ, free.flatMap(id => List(TypeVar(id, Polarity.Plus) -> Bottom, TypeVar(id, Polarity.Minus) -> Top))(collection.breakOut))
  }

  def reassignVar[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = {
    val vars = collect(typ)
    val index = vars.map(_.id).distinct.zipWithIndex.toMap
    bisubst(typ, vars.map { case v => v -> VarType(index(v.id)) }(collection.breakOut))
  }

  def simplify[A <: Polarity](typ: ErlType[A])(implicit A: A): ErlType[A] = reassignVar(removeVar(typ))

  implicit class PlusOp(val self: ErlType[Plus]) extends AnyVal {
    def \/(that: ErlType[Plus]): ErlType[Plus] =
      (self, that) match {
        case _ if self == that => self
        case (UnionType(xs), UnionType(ys)) => (xs ++ ys).foldLeft(Bottom)(_ \/ _)
        case (UnionType(xs), _) => xs.foldRight(that)(_ \/ _)
        case (_, UnionType(ys)) if ys.contains(self) => UnionType(ys)
        case (_, UnionType(ys)) => UnionType(self +: ys)
        case (VariantType(x), VariantType(y)) =>
          VariantType((x.keys ++ y.keys).map(l => l -> x.get(l).zip(y.get(l)).map { case (x, y) => x \/ y }.headOption.orElse(x.get(l)).getOrElse(y(l)))(collection.breakOut))
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
        case _ => UnionType(Vector(self, that))
      }
  }

  implicit class MinusOp(val self: ErlType[Minus]) extends AnyVal {
    def /\(that: ErlType[Minus]): ErlType[Minus] =
      (self, that) match {
        case _ if self == that => self
        case (IntersectionType(xs), IntersectionType(ys)) => (xs ++ ys).foldLeft(Top)(_ /\ _)
        case (IntersectionType(xs), _) => xs.foldRight(that)(_ /\ _)
        case (_, IntersectionType(ys)) if ys.contains(self) => IntersectionType(ys)
        case (_, IntersectionType(ys)) => IntersectionType(self +: ys)
        case (VariantType(x), VariantType(y)) => VariantType((x.keySet & y.keySet).map { l => l -> x(l) /\ y(l) }(collection.breakOut))
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
        case _ => IntersectionType(Vector(self, that))
      }
  }

}
