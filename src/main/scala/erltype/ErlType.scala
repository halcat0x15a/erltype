package erltype

sealed abstract class Polarity {
  type Inverse <: Polarity
  def inverse: Inverse
}

sealed abstract class Pos extends Polarity {
  type Inverse = Neg
  def inverse = Neg
}

case object Pos extends Pos

sealed abstract class Neg extends Polarity {
  type Inverse = Pos
  def inverse = Pos
}

case object Neg extends Neg

object Polarity {
  implicit val pos: Pos = Pos
  implicit val neg: Neg = Neg
}

sealed abstract class Type[A <: Polarity] {
  def show: String
}

case class IntType[A <: Polarity]() extends Type[A] {
  def show = "integer"
}

case class FloatType[A <: Polarity]() extends Type[A] {
  def show = "float"
}

case class CharType[A <: Polarity]() extends Type[A] {
  def show = "char"
}

case class StringType[A <: Polarity]() extends Type[A] {
  def show = "string"
}

case class AtomType[A <: Polarity](name: String) extends Type[A] {
  def show = s""""$name""""
}

case class ListType[A <: Polarity](typ: Type[A]) extends Type[A] {
  def show = s"[${typ.show}]"
}

case class UnionType(types: Vector[Type[Pos]]) extends Type[Pos] {
  def show = if (types.isEmpty) "bottom" else types.map(_.show).mkString(" | ")
}

case class IntersectionType(types: Vector[Type[Neg]]) extends Type[Neg] {
  def show = if (types.isEmpty) "top" else types.map(_.show).mkString(" & ")
}

case class FunctionType[A <: Polarity](params: List[Type[A#Inverse]], ret: Type[A]) extends Type[A] {
  def show = {
    val ps = params.map(_.show).mkString("(", ", ", ")")
    s"$ps -> ${ret.show}"
  }
}

case class VarType[A <: Polarity](id: Long) extends Type[A] {
  def show = alphabets(id)
  private[this] def alphabets(n: Long): String = (if (n / 26 > 0) alphabets(n / 26 - 1) else "") + ('A' + n % 26).toChar
}

case class SubstKey(id: Long, polarity: Polarity)

sealed abstract class BiSubst

case class BiSubst_+(id: Long, typ: Type[Pos]) extends BiSubst

case class BiSubst_-(id: Long, typ: Type[Neg]) extends BiSubst

class BiSubsts(val self: Vector[BiSubst]) extends AnyVal {
  def apply[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = {
    self.foldLeft(typ) { (typ, bisubst) =>
      bisubst match {
        case BiSubst_+(id, plus) => Type.bisubst(id, typ, plus)
        case BiSubst_-(id, minus) => Type.bisubst(id, typ, minus)
      }
    }
  }
  def compose(that: BiSubsts): BiSubsts = new BiSubsts(self ++ that.self)
}

object BiSubsts {
  val identity: BiSubsts = new BiSubsts(Vector.empty)
  def apply(bisubst: BiSubst): BiSubsts = new BiSubsts(Vector(bisubst))
}

object Type {

  def collect[A <: Polarity](typ: Type[A])(implicit A: A): Vector[SubstKey] =
    typ match {
      case ListType(typ) =>
        collect(typ)
      case FunctionType(params, ret) =>
        params.foldLeft(Vector.empty[SubstKey])(_ ++ collect(_)(A.inverse)) ++ collect(ret)
      case UnionType(types) =>
        types.foldLeft(Vector.empty[SubstKey])(_ ++ collect(_))
      case IntersectionType(types) =>
        types.foldLeft(Vector.empty[SubstKey])(_ ++ collect(_))
      case VarType(id) =>
        Vector(SubstKey(id, A))
      case _ =>
        Vector.empty
    }

  def isFreeVar[A <: Polarity](id: Long, typ: Type[A])(implicit A: A): Boolean = collect(typ).exists(_.id == id)

  def bisubst[A <: Polarity](typ: Type[A], subst: Map[SubstKey, Type[_ <: Polarity]])(implicit A: A): Type[A] =
    typ match {
      case ListType(typ) =>
        ListType(bisubst(typ, subst))
      case FunctionType(params, ret) =>
        FunctionType(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case UnionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(BottomType)(_ \/ _)
      case IntersectionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(TopType)(_ /\ _)
      case VarType(id) =>
        subst.get(SubstKey(id, A)).fold(typ)(_.asInstanceOf[Type[A]])
      case _ =>
        typ
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: Type[A], y: Type[B])(implicit A: A, B: B): Type[A] = bisubst(x, Map(SubstKey(id, B) -> y))

  def biunify(x: Type[Neg], y: Type[Pos]): BiSubsts =
    (x, y) match {
      case _ if x == y =>
        BiSubsts.identity
      case (ListType(x), ListType(y)) =>
        biunify(x, y)
      case (FunctionType(xs, x), FunctionType(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (IntersectionType(xs), _) =>
        xs.foldRight(BiSubsts.identity) { (x, f) => f compose biunify(f(x), f(y)) }
      case (_, UnionType(ys)) =>
        ys.foldRight(BiSubsts.identity) { (y, f) => f compose biunify(f(x), f(y)) }
      case (VarType(id), _) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_+(id, y \/ VarType(id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, y, VarType[Pos](fresh)) \/ VarType(id)))
      case (_, VarType(id)) =>
        if (isFreeVar(id, x))
          BiSubsts(BiSubst_-(id, x /\ VarType(id)))
        else
          BiSubsts(BiSubst_-(id, bisubst(id, x, VarType[Neg](fresh)) /\ VarType(id)))
      case _ =>
        throw new RuntimeException(s"$x is not $y")
    }

  def removeVar[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = {
    val vars = collect(typ).groupBy(_.polarity).mapValues(_.map(_.id).toSet)
    val ps = vars.getOrElse(Pos, Set.empty)
    val ms = vars.getOrElse(Neg, Set.empty)
    val free = (ps | ms) -- (ps & ms)
    bisubst(typ, free.flatMap(id => List(SubstKey(id, Pos) -> BottomType, SubstKey(id, Neg) -> TopType))(collection.breakOut))
  }

  def reassignVar[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = {
    val vars = collect(typ)
    val index = vars.map(_.id).distinct.zipWithIndex.toMap
    bisubst(typ, vars.map { case v => v -> VarType(index(v.id)) }(collection.breakOut))
  }

  def simplify[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = reassignVar(removeVar(typ))

  implicit class PosOp(val self: Type[Pos]) extends AnyVal {
    def \/(that: Type[Pos]): Type[Pos] =
      (self, that) match {
        case _ if self == that => self
        case (UnionType(xs), UnionType(ys)) => (xs ++ ys).foldLeft(BottomType)(_ \/ _)
        case (UnionType(xs), _) => xs.foldRight(that)(_ \/ _)
        case (_, UnionType(ys)) if ys.contains(self) => UnionType(ys)
        case (_, UnionType(ys)) => UnionType(self +: ys)
        case (ListType(x), ListType(y)) => ListType(x \/ y)
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
        case _ => UnionType(Vector(self, that))
      }
  }

  implicit class NegOp(val self: Type[Neg]) extends AnyVal {
    def /\(that: Type[Neg]): Type[Neg] =
      (self, that) match {
        case _ if self == that => self
        case (IntersectionType(xs), IntersectionType(ys)) => (xs ++ ys).foldLeft(TopType)(_ /\ _)
        case (IntersectionType(xs), _) => xs.foldRight(that)(_ /\ _)
        case (_, IntersectionType(ys)) if ys.contains(self) => IntersectionType(ys)
        case (_, IntersectionType(ys)) => IntersectionType(self +: ys)
        case (ListType(x), ListType(y)) => ListType(x /\ y)
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
        case _ => IntersectionType(Vector(self, that))
      }
  }

}
