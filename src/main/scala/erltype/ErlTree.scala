package erltype

sealed abstract class Tree {
  def check_+(env: Pi): TypingScheme[Type[Pos]]
  def check_-(env: Delta): TypingScheme[Type[Neg]]
}

case class IntTree(value: Int) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, IntType())
  def check_-(env: Delta) = TypingScheme(env, IntType())
}

case class FloatTree(value: Float) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, FloatType())
  def check_-(env: Delta) = TypingScheme(env, FloatType())
}

case class CharTree(value: Char) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, CharType())
  def check_-(env: Delta) = TypingScheme(env, CharType())
}

case class AtomTree(value: String) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, AtomType(value))
  def check_-(env: Delta) = TypingScheme(env, AtomType(value))
}

case class StringTree(value: String) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map.empty, StringType())
  def check_-(env: Delta) = TypingScheme(env, StringType())
}

case class VarTree(id: Long) extends Tree {
  def check_+(env: Pi) = TypingScheme(Map(id -> VarType(id)), VarType(id))
  def check_-(env: Delta) = TypingScheme(env - id, env.getOrElse(id, TopType))
}

case class ListTree(exprs: List[Tree], tail: Option[Tree]) extends Tree {
  def check_+(env: Pi) = {
    for {
      h <- Tree.checkAll_+(env, exprs)
      t <- tail.fold(TypingScheme(Map.empty, BottomType))(_.check_+(env))
    } yield ListType(h.foldLeft(BottomType)(_ \/ _)) \/ t
  }
  def check_-(env: Delta) = {
    Tree.checkAll_-(env, exprs) match {
      case TypingScheme(delta, types) =>
        tail.fold(TypingScheme(delta, TopType))(_.check_-(delta)).map(ListType(types.foldLeft(TopType)(_ /\ _)) /\ _)
    }
  }
}

case class FunClauseTree(args: List[Tree], guards: List[List[Tree]], body: List[Tree]) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, ret) = Tree.checkAll_+(env, body).map(_.lastOption.getOrElse(BottomType))
    args.foldRight(TypingScheme(delta, List.empty[Type[Neg]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(FunctionType[Pos](_, ret): Type[Pos])
  }
  def check_-(env: Delta) = throw new RuntimeException
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
        val scheme = TypingScheme(delta, rec: Type[Pos]).inst(inv, typ)
        (name.fold(env)(name => env.updated(s"$name/$arity", scheme)), scheme)
    }._2
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunRefTree(name: String, arity: Int) extends Tree {
  def check_+(env: Pi) = env(s"$name/$arity")
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunCallTree(fun: Tree, args: List[Tree]) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (f, types)) = for {
      types <- Tree.checkAll_+(env, args)
      f <- fun.check_+(env)
      f <- f match {
        case AtomType(name) => env(s"$name/${args.size}")
        case _ => TypingScheme(Map.empty, f)
      }
    } yield (f, types)
    val v = fresh
    TypingScheme[Type[Pos]](delta, VarType(v)).inst(FunctionType[Neg](types, VarType(v)), f)
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class AssignTree(lhs: Tree, rhs: Tree) extends Tree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (l, r)) = for {
      r <- rhs.check_+(env)
      l <- lhs.check_+(env)
    } yield (l, r)
    TypingScheme(delta, l).inst(lhs.check_-(delta).typ, r)
  }
  def check_-(env: Delta) = {
    rhs.check_-(env) match {
      case TypingScheme(delta, r) =>
        lhs.check_-(delta) match {
          case TypingScheme(delta, l) =>
            TypingScheme(delta, l).inst(r, lhs.check_+(Map.empty).typ)
        }
    }
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

  def checkAll_-(env: Delta, tree: Seq[Tree]): TypingScheme[List[Type[Neg]]] = {
    tree.foldRight(TypingScheme(env, List.empty[Type[Neg]])) {
      case (expr, TypingScheme(env, types)) => expr.check_-(env).map(_ :: types)
    }
  }

}
