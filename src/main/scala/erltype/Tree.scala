package erltype

sealed abstract class Tree[A <: Polarity] extends Product with Serializable

case class IntTree[A <: Polarity](value: Int) extends Tree[A]

case class FloatTree[A <: Polarity](value: Float) extends Tree[A]

case class CharTree[A <: Polarity](value: Char) extends Tree[A]

case class AtomTree[A <: Polarity](value: String) extends Tree[A]

case class StringTree[A <: Polarity](value: String) extends Tree[A]

case class VarTree[A <: Polarity](id: Long) extends Tree[A]

case class ListTree[A <: Polarity](exprs: List[Tree[A]], tail: Option[Tree[A]]) extends Tree[A]

case class TupleTree[A <: Polarity](exprs: List[Tree[A]]) extends Tree[A]

case class IfTree(clauses: List[IfClauseTree]) extends Tree[Pos]

case class IfClauseTree(guard: List[Tree[Pos]], body: List[Tree[Pos]]) extends Tree[Pos]

case class FunClauseTree(name: Option[String], args: List[Tree[Neg]], clause: IfClauseTree) extends Tree[Pos]

case class FunTree(clauses: List[FunClauseTree]) extends Tree[Pos]

case class FunRefTree(name: String, arity: Int) extends Tree[Pos]

case class FunCallTree(fun: Tree[Pos], args: List[Tree[Pos]]) extends Tree[Pos]

case class MatchTree[A <: Polarity](lhs: Tree[Neg], rhs: Tree[A]) extends Tree[A]

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
      case IfTree(clauses) =>
        checkAll(env, clauses).map(sum)
      case IfClauseTree(guards, body) =>
        for {
          _ <- TypingScheme.inst(checkAll(env, guards).map { bools =>
            (BooleanType[Pos], TupleType(bools): Type[Pos], TupleType(List.fill(guards.size)(BooleanType[Neg])): Type[Neg])
          })
          types <- checkAll(env, body)
        } yield types.lastOption.getOrElse(BottomType)
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
      case MatchTree(lhs, rhs) =>
        TypingScheme.inst(for {
          (delta, pos) <- rhs.check(env).get
          neg <- lhs.pattern(delta)
        } yield (pos, pos, neg))
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
