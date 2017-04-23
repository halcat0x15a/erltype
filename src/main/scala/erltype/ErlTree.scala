package erltype

sealed abstract class Tree {
  def check_+(env: Pi): TypingScheme[Type[Pos]]
  def check_- : SchemeState[Type[Neg]]
}

case class IntTree(value: Int) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, IntType())
  def check_- = SchemeState(IntType())
}

case class FloatTree(value: Float) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, FloatType())
  def check_- = SchemeState(FloatType())
}

case class CharTree(value: Char) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, CharType())
  def check_- = SchemeState(CharType())
}

case class AtomTree(value: String) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, AtomType(value))
  def check_- = SchemeState(AtomType(value))
}

case class StringTree(value: String) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, StringType())
  def check_- = SchemeState(StringType())
}

case class VarTree(id: Long) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map(id -> VarType(id)), VarType(id))
  def check_- = new SchemeState[Type[Neg]] {
    def apply(env: Delta) = TypingScheme(env - id, env.getOrElse(id, TopType))
  }
}

case class ListTree(exprs: List[Tree], tail: Option[Tree]) extends Tree {
  def check_+(env: Pi) = {
    for {
      types <- Tree.checkAll_+(env, exprs)
      tail <- tail.fold(TypingScheme(Map.empty, BottomType))(_.check_+(env))
    } yield ListType(types.foldLeft(BottomType)(_ \/ _)) \/ tail
  }
  def check_- = {
    for {
      types <- Tree.checkAll_-(exprs)
      tail <- tail.fold(SchemeState(TopType))(_.check_-)
    } yield ListType(types.foldLeft(TopType)(_ /\ _)) /\ tail
  }
}

case class FunClauseTree(args: List[Tree], guards: List[List[Tree]], body: List[Tree]) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, ret) = for {
      guards <- Tree.checkAll_+(env, guards.flatten)
      _ <- TypingScheme(Map.empty, BooleanType[Pos]).inst(TupleType(guards), TupleType(List.fill(guards.size)(BooleanType[Neg])))
      types <- Tree.checkAll_+(env, body)
    } yield types.lastOption.getOrElse(BottomType)
    Tree.checkAll_-(args)(delta).map(FunctionType[Pos](_, ret))
  }
  def check_- = throw new RuntimeException
}

case class FunTree(name: Option[String], clauses: List[FunClauseTree]) extends Tree {
  def check_+(env: Pi) = {
    val arity = clauses(0).args.size
    val args = List.fill(arity)(VarType[Neg](fresh))
    val ret = VarType[Pos](fresh)
    val rec = FunctionType[Pos](args, ret)
    val inv = FunctionType[Neg](args.map(arg => VarType[Pos](arg.id)), VarType[Neg](ret.id))
    val ext = env ++ name.map(name => Map(s"$name/$arity" -> TypingScheme(Map.empty, rec: Type[Pos]))).getOrElse(Map.empty)
    clauses.foldLeft((ext, TypingScheme(Map.empty, rec: Type[Pos]))) {
      case ((env, _), clause) =>
        val TypingScheme(delta, typ) = clause.check_+(env)
        val scheme = TypingScheme(delta, rec: Type[Pos]).inst(typ, inv)
        (name.fold(env)(name => env.updated(s"$name/$arity", scheme)), scheme)
    }._2
  }
  def check_- = throw new RuntimeException
}

case class FunRefTree(name: String, arity: Int) extends Tree {
  def check_+(env: Pi) = env(s"$name/$arity")
  def check_- = throw new RuntimeException
}

case class FunCallTree(fun: Tree, args: List[Tree]) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (f, types)) = for {
      types <- Tree.checkAll_+(env, args)
      f <- fun.check_+(env)
    } yield (f, types)
    val v = fresh
    TypingScheme[Type[Pos]](delta, VarType(v)).inst(f, FunctionType[Neg](types, VarType(v)))
  }
  def check_- = throw new RuntimeException
}

case class AssignTree(lhs: Tree, rhs: Tree) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (r, l)) = rhs.check_+(env).zip(lhs.check_-)
    TypingScheme(delta, r).inst(r, l)
  }
  def check_- = {
    for {
      r <- rhs.check_-
      typ <- new SchemeState[Type[Neg]] {
        def apply(env: Delta) = lhs.check_+(Map.empty).flatMap(l => TypingScheme(env, r).inst(l, r)) // TODO
      }
    } yield typ
  }
}

object Tree {

  def checkAll_+(env: Pi, tree: Seq[Tree]): TypingScheme[List[Type[Pos]]] = {
    tree.foldRight(TypingScheme(Map.empty, List.empty[Type[Pos]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check_+(env)
      } yield typ :: types
    }
  }

  def checkAll_-(tree: Seq[Tree]): SchemeState[List[Type[Neg]]] = {
    tree.foldRight(SchemeState(List.empty[Type[Neg]])) { (expr, state) =>
      for {
        types <- state
        typ <- expr.check_-
      } yield typ :: types
    }
  }

}
