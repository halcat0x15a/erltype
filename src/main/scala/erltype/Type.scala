package erltype

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

  def show: String

  def inverse: Type[A#Inverse] = this.asInstanceOf[Type[A#Inverse]]

  def \/(that: Type[A]): Type[A] = (this, that) match {
    case _ if this == that => this
    case (ListType(x), ListType(y)) => ListType(x \/ y)
    case (TupleType(xs), TupleType(ys)) => TupleType(xs.zip(ys).map { case (x, y) => x \/ y })
    case (FunType(xs, x), FunType(ys, y)) => FunType(xs.zip(ys).map { case (x, y) => x /\ y }, x \/ y)
    case (UnionType(_), UnionType(ys)) => ys.foldLeft(this)(_ \/ _)
    case (UnionType(xs), _) =>
      val (types, merged) = xs.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), x) => x \/ that match {
          case UnionType(_) => (types :+ x, merged)
          case xy => (types :+ xy, true)
        }
      }
      UnionType(if (merged) types else types :+ that)
    case (_, UnionType(ys)) =>
      val (types, merged) = ys.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), y) => this \/ y match {
          case UnionType(_) => (types :+ y, merged)
          case xy => (types :+ xy, true)
        }
      }
      UnionType(if (merged) types else this +: types)
    case (IntersectionType(xs), IntersectionType(ys)) if xs.toSet.subsetOf(ys.toSet) => this
    case (IntersectionType(xs), IntersectionType(ys)) if ys.toSet.subsetOf(xs.toSet) => that
    case (_, IntersectionType(ys)) if ys.contains(this) => this
    case (IntersectionType(xs), _) if xs.contains(that) => that
    case _ => UnionType(Vector(this, that))
  }

  def /\(that: Type[A]): Type[A] = (this, that) match {
    case _ if this == that => this
    case (ListType(x), ListType(y)) => ListType(x /\ y)
    case (TupleType(xs), TupleType(ys)) => TupleType(xs.zip(ys).map { case (x, y) => x /\ y })
    case (FunType(xs, x), FunType(ys, y)) => FunType(xs.zip(ys).map { case (x, y) => x \/ y }, x /\ y)
    case (UnionType(xs), UnionType(ys)) => (for {
      y <- ys
      x <- xs
    } yield x /\ y).foldLeft(BottomType[A])(_ \/ _)
    case (UnionType(xs), _) =>
      val (types, merged) = xs.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), x) => x /\ that match {
          case UnionType(_) => (types :+ x, merged)
          case xy => (types :+ xy, true)
        }
      }
      UnionType(if (merged) types else types :+ that)
    case (_, UnionType(ys)) =>
      val (types, merged) = ys.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), y) => this /\ y match {
          case UnionType(_) => (types :+ y, merged)
          case xy => (types :+ xy, true)
        }
      }
      UnionType(if (merged) types else this +: types)
    case (IntersectionType(_), IntersectionType(ys)) => ys.foldLeft(this)(_ /\ _)
    case (IntersectionType(xs), _) =>
      val (types, merged) = xs.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), x) => x /\ that match {
          case IntersectionType(_) => (types :+ x, merged)
          case xy => (types :+ xy, true)
        }
      }
      IntersectionType(if (merged) types else types :+ that)
    case (_, IntersectionType(ys)) =>
      val (types, merged) = ys.foldLeft((Vector.empty[Type[A]], false)) {
        case ((types, merged), y) => this /\ y match {
          case IntersectionType(_) => (types :+ y, merged)
          case xy => (types :+ xy, true)
        }
      }
      IntersectionType(if (merged) types else this +: types)
    case _ => IntersectionType(Vector(this, that))
  }

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

case class TupleType[A <: Polarity](types: Vector[Type[A]]) extends Type[A] {
  def show = s"{${types.map(_.show).mkString}}"
}

case class FunType[A <: Polarity](params: Vector[Type[A#Inverse]], ret: Type[A]) extends Type[A] {
  def show = {
    val ps = params.map(_.show).mkString("(", ", ", ")")
    s"$ps -> ${ret.show}"
  }
}

case class UnionType[A <: Polarity](types: Vector[Type[A]]) extends Type[A] {
  def show = if (types.isEmpty) "bottom" else types.map(_.show).mkString(" | ")
}

case class IntersectionType[A <: Polarity](types: Vector[Type[A]]) extends Type[A] {
  def show = if (types.isEmpty) "top" else types.map(_.show).mkString(" & ")
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

  def inverseAll[F[_], A <: Polarity](types: F[Type[A]]): F[Type[A#Inverse]] = types.asInstanceOf[F[Type[A#Inverse]]]

  def collect[A <: Polarity](typ: Type[A])(implicit A: A): Vector[SubstKey] =
    typ match {
      case ListType(typ) =>
        collect(typ)
      case TupleType(types) =>
        types.foldLeft(Vector.empty[SubstKey])(_ ++ collect(_))
      case FunType(params, ret) =>
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
      case FunType(params, ret) =>
        FunType(params.map(bisubst(_, subst)(A.inverse)), bisubst(ret, subst))
      case UnionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(BottomType[A])(_ \/ _)
      case IntersectionType(types) =>
        types.map(bisubst(_, subst)).foldLeft(TopType[A])(_ /\ _)
      case VarType(id) =>
        subst.get(SubstKey(id, A)).orElse(subst.get(SubstKey(id, A.inverse))).fold(typ)(_.asInstanceOf[Type[A]])
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
      case (FunType(xs, x), FunType(ys, y)) =>
        val f = xs.zip(ys).foldLeft(BiSubsts.identity) { case (f, (x, y)) => f compose biunify(f(y), f(x)) }
        f compose biunify(f(x), f(y))
      case (_, UnionType(ys)) if ys.size == 1 =>
        biunify(x, ys.head)
      case (UnionType(xs), UnionType(ys)) if inverseAll(ys).toSet.subsetOf(xs.toSet) =>
        BiSubsts.identity
      case (UnionType(xs), _) =>
        xs.foldRight(BiSubsts.identity) { (x, f) => f compose biunify(f(x), f(y)) }
      case (IntersectionType(xs), _) if xs.size == 1 =>
        biunify(xs.head, y)
      case (IntersectionType(xs), IntersectionType(ys)) if inverseAll(ys).toSet.subsetOf(xs.toSet) =>
        BiSubsts.identity
      case (IntersectionType(xs), _) if xs.contains(y) =>
        BiSubsts.identity
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

  def removeVars[A <: Polarity](typ: Type[A], vars: Set[Long], nil: Type[A]): Type[A] =
    typ match {
      case ListType(typ) =>
        ListType(removeVars(typ, vars, nil))
      case TupleType(types) =>
        TupleType(types.map(removeVars(_, vars, nil)))
      case FunType(params, ret) =>
        FunType(params.map(removeVars(_, vars, nil.asInstanceOf[Type[A#Inverse]])), removeVars(ret, vars, nil))
      case UnionType(types) =>
        types.map(removeVars(_, vars, BottomType)).foldLeft(BottomType[A])(_ \/ _)
      case IntersectionType(types) =>
        types.map(removeVars(_, vars, TopType)).foldLeft(TopType[A])(_ /\ _)
      case VarType(id) if vars.contains(id) => nil
      case _ =>
        typ
    }

  def removeVars[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = {
    val vars = collect(typ).groupBy(_.polarity).mapValues(_.map(_.id).toSet)
    val ps = vars.getOrElse(Pos, Set.empty)
    val ms = vars.getOrElse(Neg, Set.empty)
    val free = (ps | ms) -- (ps & ms)
    //bisubst(typ, free.flatMap(id => List(SubstKey(id, Pos) -> BottomType, SubstKey(id, Neg) -> TopType))(collection.breakOut))
    removeVars(typ, free, if (A == Pos) BottomType else TopType)
  }

  def reassignVar[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = {
    val vars = collect(typ)
    val index = vars.map(_.id).distinct.zipWithIndex.toMap
    bisubst(typ, vars.map(v => v -> VarType(index(v.id)))(collection.breakOut))
  }

  def simplify[A <: Polarity](typ: Type[A])(implicit A: A): Type[A] = reassignVar(removeVars(typ))

}
