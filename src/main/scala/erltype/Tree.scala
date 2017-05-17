package erltype

sealed abstract class Tree {
  def check(env: Pi): TypingScheme[Type[Pos]]
}

sealed abstract class PatternTree extends Tree {
  def pattern(env: Delta): TypingScheme[Type[Neg]] = ???
}

case class IntTree(value: Int) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map.empty, IntType())
}

case class FloatTree(value: Float) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map.empty, FloatType())
}

case class CharTree(value: Char) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map.empty, CharType())
}

case class AtomTree(value: String) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map.empty, AtomType(value))
}

case class StringTree(value: String) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map.empty, StringType())
}

case class VarTree(id: Long) extends PatternTree {
  def check(env: Pi) = TypingScheme(Map(id -> VarType(id)), VarType(id))
}

case class ListTree(exprs: List[Tree], tail: Option[Tree]) extends PatternTree {
  def check(env: Pi) = {
    val TypingScheme(delta, result) = for {
      types <- Tree.checkAll(env, exprs)
      tail <- tail.fold(TypingScheme(Map.empty, BottomType))(_.check(env))
    } yield ListType(types.foldLeft(BottomType)(_ \/ _)) \/ tail
    TypingScheme(delta, result).inst(result, ListType(VarType(fresh)))
  }
}

case class FunClauseTree(name: Option[String], args: List[Tree], guards: List[List[Tree]], body: List[Tree]) extends Tree {
  def check(env: Pi) = {
    val arity = args.size
    val rec: Type[Pos] = FunctionType(List.fill(arity)(VarType(fresh)), VarType(fresh))
    val ext = name.fold(env)(name => env + (s"$name/$arity" -> TypingScheme(Map.empty, rec)))
    val ret = for {
      guards <- Tree.checkAll(ext, guards.flatten)
      _ <- TypingScheme(Map.empty, BooleanType[Pos]).inst(TupleType(guards), TupleType(List.fill(guards.size)(BooleanType[Neg])))
      types <- Tree.checkAll(ext, body)
    } yield types.lastOption.getOrElse(BottomType)
    val TypingScheme(delta, types) = ret.flatMap(_ => Tree.checkAll(Map.empty, args))
    val params = types.map(TypingScheme.get(delta, _))
    //println(delta.map { case (k, v) => VarType(k).show -> v.show })
    //println(rec.show, types.map(_.show))
    val result = TypingScheme(delta -- params.flatMap(Type.collect(_)).map(_.id), FunctionType[Pos](params, ret.typ): Type[Pos])
    result.inst(result.typ, rec.inverse)
  }
}

case class FunTree(clauses: List[FunClauseTree]) extends Tree {
  def check(env: Pi) = {
    clauses.foldLeft(TypingScheme(Map.empty, BottomType)) { (scheme, clause) =>
      for {
        x <- scheme
        y <- clause.check(env)
        _ = println(y.show)
      } yield x \/ y
    }
  }
}

case class FunRefTree(name: String, arity: Int) extends Tree {
  def check(env: Pi) = env(s"$name/$arity").map(Type.alpha)
}

case class FunCallTree(fun: Tree, args: List[Tree]) extends Tree {
  def check(env: Pi) = {
    val TypingScheme(delta, (f, types)) = for {
      types <- Tree.checkAll(env, args)
      f <- fun.check(env)
    } yield (f, types)
    val v = fresh
    TypingScheme[Type[Pos]](delta, VarType(v)).inst(f, FunctionType[Neg](types, VarType(v)))
  }
}

case class MatchTree(lhs: Tree, rhs: Tree) extends Tree {
  def check(env: Pi) = ???
}

object Tree {
  def checkAll(env: Pi, tree: Seq[Tree]): TypingScheme[List[Type[Pos]]] = {
    tree.foldRight(TypingScheme(Map.empty, List.empty[Type[Pos]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check(env)
      } yield typ :: types
    }
  }
}
