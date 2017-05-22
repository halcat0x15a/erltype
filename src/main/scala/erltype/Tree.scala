package erltype

sealed abstract class Tree[A <: Polarity] extends Product with Serializable {
  def show: String
}

case class IntTree[A <: Polarity](value: Int) extends Tree[A] {
  def show = value.toString
}

case class FloatTree[A <: Polarity](value: Float) extends Tree[A] {
  def show = value.toString
}

case class CharTree[A <: Polarity](value: Char) extends Tree[A] {
  def show = value.toString
}

case class AtomTree[A <: Polarity](value: String) extends Tree[A] {
  def show = s"'$value'"
}

case class StringTree[A <: Polarity](value: String) extends Tree[A] {
  def show = s"""$value"""
}

case class VarTree[A <: Polarity](id: Long) extends Tree[A] {
  def show = alphabets(id)
}

case class ListTree[A <: Polarity](exprs: List[Tree[A]], tail: Option[Tree[A]]) extends Tree[A] {
  def show = {
    val h = exprs.map(_.show).mkString(", ")
    val t = tail.fold("")(e => s" | ${e.show}")
    s"[$h$t]"
  }
}

case class TupleTree[A <: Polarity](exprs: List[Tree[A]]) extends Tree[A] {
  def show = exprs.map(_.show).mkString("{", ", ", "}")
}

case class BlockTree(exprs: List[Tree[Pos]]) extends Tree[Pos] {
  def show = exprs.map(_.show).mkString(", ")
}

case class IfTree(clauses: List[IfClauseTree]) extends Tree[Pos] {
  def show = {
    val cs = clauses.map(_.show).mkString("; ")
    s"if $cs end"
  }
}

case class IfClauseTree(guard: List[Tree[Pos]], body: Tree[Pos]) extends Tree[Pos] {
  def show = {
    val w = if (guard.isEmpty) "" else "when "
    val g = guard.map(_.show).mkString(", ")
    s"$w$g-> ${body.show}"
  }
}

case class FunClauseTree(name: Option[String], args: List[Tree[Neg]], clause: IfClauseTree) extends Tree[Pos] {
  def show = {
    val n = name.getOrElse("fun")
    val as = args.map(_.show).mkString(", ")
    s"$n($as) ${clause.show} end"
  }
}

case class FunTree(clauses: List[FunClauseTree]) extends Tree[Pos] {
  def show = clauses.map(_.show).mkString(";\n")
}

case class FunRefTree(name: String, arity: Int) extends Tree[Pos] {
  def show = s"(fun $name/$arity)"
}

case class FunCallTree(fun: Tree[Pos], args: List[Tree[Pos]]) extends Tree[Pos] {
  def show = {
    val as = args.map(_.show).mkString(", ")
    s"${fun.show}($as)"
  }
}

case class MatchTree(lhs: Tree[Neg], rhs: Tree[Neg]) extends Tree[Neg] {
  def show = s"${lhs.show} = ${rhs.show}"
}

object Tree {

  def checkAll(env: Pi, tree: Seq[Tree[Pos]]): TypingScheme[List[Type[Pos]]] = {
    tree.foldRight(TypingScheme(List.empty[Type[Pos]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check(env)
      } yield typ :: types
    }
  }

  def patternAll(env: Delta, tree: Seq[Tree[Neg]]): TypingScheme[List[Type[Neg]]] = {
    tree.foldRight(TypingScheme(List.empty[Type[Neg]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.pattern(env)
      } yield typ :: types
    }
  }

  implicit class PosOp(self: Tree[Pos]) {
    def check(env: Pi): TypingScheme[Type[Pos]] = self match {
      case IntTree(_) =>
        TypingScheme(IntType())
      case FloatTree(_) =>
        TypingScheme(FloatType())
      case CharTree(_) =>
        TypingScheme(CharType())
      case AtomTree(name) =>
        TypingScheme(AtomType(name))
      case StringTree(_) =>
        TypingScheme(StringType())
      case VarTree(id) =>
        TypingScheme(Map(id -> VarType(id)), VarType(id))
      case ListTree(exprs, tail) =>
        TypingScheme.inst(for {
          types <- checkAll(env, exprs)
          tail <- tail.fold(TypingScheme(BottomType))(_.check(env))
        } yield {
          val list = ListType(sum(types)) \/ tail
          (list, list, ListType(VarType(fresh)))
        })
      case TupleTree(exprs) =>
        checkAll(env, exprs).map(TupleType(_))
      case BlockTree(exprs) => checkAll(env, exprs.reverse).map(_.headOption.getOrElse(BottomType))
      case IfTree(clauses) =>
        checkAll(env, clauses).map(sum)
      case IfClauseTree(guards, body) =>
        for {
          _ <- TypingScheme.inst(checkAll(env, guards).map { bools =>
            (BooleanType[Pos], TupleType(bools): Type[Pos], TupleType(List.fill(guards.size)(BooleanType[Neg])): Type[Neg])
          })
          typ <- body.check(env)
        } yield typ
      case FunClauseTree(name, args, clause) =>
        val arity = args.size
        val rec: Type[Pos] = FunctionType(List.fill(arity)(VarType(fresh)), VarType(fresh))
        val ext = name.fold(env)(name => env + (s"$name/$arity" -> TypingScheme(rec)))
        val x = TypingScheme.simplify(TypingScheme.inst(for {
          (delta, ret) <- clause.check(ext).get
          params <- patternAll(delta, args)
        } yield {
          val fun = FunctionType[Pos](params, ret): Type[Pos]
          (fun, fun, rec.inverse)
        }))
        x
      case FunTree(clauses) =>
        //checkAll(env, clauses).map(sum)
        clauses.foldLeft(TypingScheme(BottomType)) { (scheme, expr) =>
          for {
            x <- scheme
            y <- expr.check(env)
          } yield x \/ y
        }
      case FunRefTree(name, arity) =>
        env(s"$name/$arity").map(Type.alpha)
      case FunCallTree(fun, args) =>
        val v = fresh
        TypingScheme.inst(for {
          pos <- fun.check(env)
          types <- Tree.checkAll(env, args)
        } yield (VarType[Pos](v), pos, FunctionType[Neg](types, VarType(v))))
    }
  }

  implicit class NegOp(self: Tree[Neg]) {
    def pattern(env: Delta): TypingScheme[Type[Neg]] = self match {
      case IntTree(_) =>
        TypingScheme(env, IntType())
      case FloatTree(_) =>
        TypingScheme(env, FloatType())
      case CharTree(_) =>
        TypingScheme(env, CharType())
      case AtomTree(name) =>
        TypingScheme(env, AtomType(name))
      case StringTree(_) =>
        TypingScheme(env, StringType())
      case VarTree(id) =>
        TypingScheme(env, Vector(id), env.getOrElse(id, VarType(id)))
      case TupleTree(exprs) =>
        patternAll(env, exprs).map(TupleType(_))
      case ListTree(exprs, tail) =>
        TypingScheme.inst(for {
          types <- patternAll(env, exprs)
          tail <- tail.fold(TypingScheme(TopType))(_.pattern(env))
        } yield {
          val list = ListType(prod(types)) /\ tail
          (list, ListType(VarType(fresh)), list)
        })
      case MatchTree(lhs, rhs) =>
        TypingScheme.inst(for {
          l <- lhs.pattern(env)
          r <- rhs.pattern(env)
        } yield (l, l.inverse, r))
    }
  }

}
