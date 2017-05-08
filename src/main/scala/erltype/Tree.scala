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

case class WildCardTree() extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, BottomType)
  def check_- = SchemeState(TopType)
}

case class ListTree(exprs: List[Tree], tail: Option[Tree]) extends Tree {
  def check_+(env: Pi) = {
    for {
      types <- Tree.checkAll_+(env, exprs)
      tail <- tail.fold(TypingScheme(Map.empty, BottomType[Pos]))(_.check_+(env))
    } yield ListType(types.foldLeft(BottomType[Pos])(_ \/ _)) \/ tail
  }
  def check_- = {
    for {
      types <- Tree.checkAll_-(exprs)
      tail <- tail.fold(SchemeState(TopType[Neg]))(_.check_-)
    } yield ListType(types.foldLeft(TopType[Neg])(_ /\ _)) /\ tail
  }
}

case class FunClauseTree(args: List[Tree], guards: List[List[Tree]], body: List[Tree]) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, ret) = for {
      guards <- Tree.checkAll_+(env, guards.flatten)
      _ <- TypingScheme(Map.empty, BooleanType[Pos]).inst(TupleType(guards), TupleType(Vector.fill(guards.size)(BooleanType[Neg])))
      types <- Tree.checkAll_+(env, body)
    } yield types.lastOption.getOrElse(BottomType)
    Tree.checkAll_-(args)(delta).map(FunType[Pos](_, ret))
  }
  def check_- = throw new RuntimeException
}

case class FunTree(name: Option[String], clauses: List[FunClauseTree]) extends Tree {
  def check_+(env: Pi) = {
    val arity = clauses(0).args.size
    val args = Vector.fill(arity)(VarType[Neg](fresh))
    val ret = VarType[Pos](fresh)
    val rec: Type[Pos] = FunType[Pos](args, ret)
    val ext = name.fold(env)(name => env + (s"$name/$arity" -> TypingScheme(Map.empty, rec)))
    clauses.foldLeft(TypingScheme(Map.empty, rec)) { (scheme, clause) =>
      for {
        x <- scheme
        y <- clause.check_+(ext)
      } yield {
        println(y.show)
        (x, y) match {
          case (FunType(xs, x), FunType(ys, y)) =>
            FunType(xs.zip(ys).map { case (x, y) => x \/ y }, x \/ y)
          case _ => throw new RuntimeException
        }
      }
    }
    /*clauses.foldLeft((ext, TypingScheme(Map.empty, rec: Type[Pos]))) {
      case ((env, _), clause) =>
        val TypingScheme(delta, typ) = clause.check_+(env)
        val scheme = TypingScheme(delta, rec: Type[Pos]).inst(typ, inv)
        (name.fold(env)(name => env.updated(s"$name/$arity", scheme)), scheme)
    }._2*/
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
    TypingScheme[Type[Pos]](delta, VarType(v)).inst(f, FunType[Neg](types, VarType(v)))
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

  def checkAll_+(env: Pi, tree: Seq[Tree]): TypingScheme[Vector[Type[Pos]]] = {
    tree.foldLeft(TypingScheme(Map.empty, Vector.empty[Type[Pos]])) { (scheme, expr) =>
      for {
        types <- scheme
        typ <- expr.check_+(env)
      } yield types :+ typ
    }
  }

  def checkAll_-(tree: Seq[Tree]): SchemeState[Vector[Type[Neg]]] = {
    tree.foldLeft(SchemeState(Vector.empty[Type[Neg]])) { (state, expr) =>
      for {
        types <- state
        typ <- expr.check_-
      } yield types :+ typ
    }
  }

}
