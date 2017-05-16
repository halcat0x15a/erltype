package erltype

import scala.collection.mutable.HashMap

sealed abstract class Polarity extends Product with Serializable {
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

sealed abstract class Type[A <: Polarity] extends Product with Serializable {
  def inverse: Type[A#Inverse]
  def show: String
}

case class IntType[A <: Polarity]() extends Type[A] {
  def inverse = IntType()
  def show = "integer"
}

case class FloatType[A <: Polarity]() extends Type[A] {
  def inverse = FloatType()
  def show = "float"
}

case class CharType[A <: Polarity]() extends Type[A] {
  def inverse = CharType()
  def show = "char"
}

case class StringType[A <: Polarity]() extends Type[A] {
  def inverse = StringType()
  def show = "string"
}

case class AtomType[A <: Polarity](name: String) extends Type[A] {
  def inverse = AtomType(name)
  def show = s""""$name""""
}

case class ListType[A <: Polarity](typ: Type[A]) extends Type[A] {
  def inverse = ListType(typ.inverse)
  def show = s"[${typ.show}]"
}

case class TupleType[A <: Polarity](types: List[Type[A]]) extends Type[A] {
  def inverse = TupleType(types.map(_.inverse))
  def show = s"{${types.map(_.show).mkString}}"
}

case class UnionType(types: Vector[Type[Pos]]) extends Type[Pos] {
  def inverse = IntersectionType(types.map(_.inverse))
  def show = if (types.isEmpty) "bottom" else types.map(_.show).mkString(" | ")
}

case class IntersectionType(types: Vector[Type[Neg]]) extends Type[Neg] {
  def inverse = UnionType(types.map(_.inverse))
  def show = if (types.isEmpty) "top" else types.map(_.show).mkString(" & ")
}

case class FunctionType[A <: Polarity](params: List[Type[A#Inverse]], ret: Type[A]) extends Type[A] {
  def inverse = FunctionType(params.map(_.inverse), ret.inverse)
  def show = s"""(${params.map(_.show).mkString(", ")}) -> ${ret.show}"""
}

case class VarType[A <: Polarity](id: Long) extends Type[A] {
  def inverse = VarType(id)
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

  def sum(types: Seq[Type[Pos]]): Type[Pos] = types.foldLeft(BottomType)(_ \/ _)

  def prod(types: Seq[Type[Neg]]): Type[Neg] = types.foldLeft(TopType)(_ /\ _)

  def alpha[A <: Polarity](typ: Type[A]): Type[A] = {
    val table = HashMap.empty[Long, Long]
    def go[A <: Polarity](typ: Type[A]): Type[A] =
      typ match {
        case ListType(typ) => ListType(go(typ))
        case TupleType(types) => TupleType(types.map(go))
        case FunctionType(params, ret) => FunctionType(params.map(go), go(ret))
        case UnionType(types) => UnionType(types.map(go))
        case IntersectionType(types) => IntersectionType(types.map(go))
        case VarType(id) => VarType(table.getOrElseUpdate(id, fresh))
        case _ => typ
      }
    go(typ)
  }

  def collect[A <: Polarity](typ: Type[A])(implicit A: A): Vector[SubstKey] =
    typ match {
      case ListType(typ) =>
        collect(typ)
      case TupleType(types) =>
        types.foldLeft(Vector.empty[SubstKey])(_ ++ collect(_))
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
      case TupleType(types) =>
        TupleType(types.map(bisubst(_, subst)))
      case FunctionType(params, ret) =>
        FunctionType(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case UnionType(types) =>
        sum(types.map(bisubst(_, subst)))
      case IntersectionType(types) =>
        prod(types.map(bisubst(_, subst)))
      case VarType(id) =>
        subst.get(SubstKey(id, A)).fold(typ)(_.asInstanceOf[Type[A]])
      case _ =>
        typ
    }

  def bisubst[A <: Polarity, B <: Polarity](id: Long, x: Type[A], y: Type[B])(implicit A: A, B: B): Type[A] = bisubst(x, Map(SubstKey(id, B) -> y))

  def biunify(x: Type[Pos], y: Type[Neg]): BiSubsts =
    (x, y) match {
      case _ if x == y =>
        BiSubsts.identity
      case (ListType(x), ListType(y)) =>
        biunify(x, y)
      case (TupleType(xs), TupleType(ys)) =>
        xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(x), f(y)) }
      case (FunctionType(xs, x), FunctionType(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (UnionType(xs), _) =>
        xs.foldRight(BiSubsts.identity) { (x, f) => f compose biunify(f(x), f(y)) }
      case (_, IntersectionType(ys)) =>
        ys.foldRight(BiSubsts.identity) { (y, f) => f compose biunify(f(x), f(y)) }
      case (VarType(id), _) =>
        if (isFreeVar(id, y))
          BiSubsts(BiSubst_-(id, y /\ VarType(id)))
        else
          BiSubsts(BiSubst_-(id, bisubst(id, y, VarType[Pos](fresh)) /\ VarType(id)))
      case (_, VarType(id)) =>
        if (isFreeVar(id, x))
          BiSubsts(BiSubst_+(id, x \/ VarType(id)))
        else
          BiSubsts(BiSubst_+(id, bisubst(id, x, VarType[Neg](fresh)) \/ VarType(id)))
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
        case (UnionType(xs), UnionType(ys)) => sum(xs ++ ys)
        case (UnionType(xs), _) =>
          UnionType(xs.foldRight(Vector(that)) { (x, types) =>
            x \/ that match {
              case UnionType(_) => x +: types
              case xy => xy +: types.init
            }
          })
        case (_, UnionType(ys)) =>
          UnionType(ys.foldLeft(Vector(self)) { (types, y) =>
            self \/ y match {
              case UnionType(_) => types :+ y
              case xy => types.tail :+ xy
            }
          })
        case (ListType(x), ListType(y)) => ListType(x \/ y)
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
        case _ => UnionType(Vector(self, that))
      }
  }

  implicit class NegOp(val self: Type[Neg]) extends AnyVal {
    def /\(that: Type[Neg]): Type[Neg] =
      (self, that) match {
        case _ if self == that => self
        case (IntersectionType(xs), IntersectionType(ys)) => prod(xs ++ ys)
        case (IntersectionType(xs), _) =>
          IntersectionType(xs.foldRight(Vector(that)) { (x, types) =>
            x /\ that match {
              case IntersectionType(_) => x +: types
              case xy => xy +: types.init
            }
          })
        case (_, IntersectionType(ys)) =>
          IntersectionType(ys.foldLeft(Vector(self)) { (types, y) =>
            self /\ y match {
              case IntersectionType(_) => types :+ y
              case xy => types.tail :+ xy
            }
          })
        case (ListType(x), ListType(y)) => ListType(x /\ y)
        case (FunctionType(xs, x), FunctionType(ys, y)) => FunctionType(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
        case _ => IntersectionType(Vector(self, that))
      }
  }

}
