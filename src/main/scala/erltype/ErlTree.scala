package erltype

sealed abstract class ErlTree {
  def check_+(env: Pi): TypingScheme[ErlType[Plus]]
  def check_-(env: Delta): TypingScheme[ErlType[Minus]]
  def checkAll(env: Pi, tree: Seq[ErlTree]): TypingScheme[List[ErlType[Plus]]] = {
    tree.foldRight(TypingScheme(Map.empty, List.empty[ErlType[Plus]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check_+(env)
      } yield typ :: types
    }
  }
}

case class IntTree(value: Int) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, IntType())
  def check_-(env: Delta) = TypingScheme(env, IntType())
}

case class FloatTree(value: Float) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, FloatType())
  def check_-(env: Delta) = TypingScheme(env, FloatType())
}

case class CharTree(value: Char) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, CharType())
  def check_-(env: Delta) = TypingScheme(env, CharType())
}

case class AtomTree(value: String) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, AtomType(value))
  def check_-(env: Delta) = TypingScheme(env, AtomType(value))
}

case class StringTree(value: String) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, StringType())
  def check_-(env: Delta) = TypingScheme(env, StringType())
}

case class VarTree(id: Long) extends ErlTree {
  def check_+(env: Pi) = {
    TypingScheme(Map(id -> VarType(id)), VarType(id))
  }
  def check_-(env: Delta) = {
    TypingScheme(env - id, env.getOrElse(id, ErlType.Top))
  }
}

sealed abstract class ListTree extends ErlTree

case object NilTree extends ListTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ListType(ErlType.Bottom))
  def check_-(env: Delta) = TypingScheme(env, ListType(ErlType.Top))
}

case class ConsTree(head: ErlTree, tail: ErlTree) extends ListTree {
  def check_+(env: Pi) = for {
    h <- head.check_+(env)
    t <- tail.check_+(env)
  } yield {
    ListType(h) \/ t
  }
  def check_-(env: Delta) = {
    head.check_-(env) match {
      case TypingScheme(env, h) =>
        tail.check_-(env) match {
          case TypingScheme(env, t) =>
            TypingScheme(env, ListType(h) /\ t)
        }
    }
  }
}

case class FunClauseTree(args: List[ErlTree], guards: List[List[ErlTree]], body: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, ret) = checkAll(env, body).map(_.lastOption.getOrElse(ErlType.Bottom))
    args.foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(FunctionType[Plus](_, ret): ErlType[Plus])
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunTree(name: Option[String], clauses: List[FunClauseTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val arity = clauses(0).args.size
    val args = List.fill(arity)(VarType[Minus](ErlType.fresh))
    val ret = VarType[Plus](ErlType.fresh)
    val rec = FunctionType[Plus](args, ret)
    val inv = FunctionType[Minus](args.map(arg => VarType[Plus](arg.id)), VarType[Minus](ret.id))
    val ext = env ++ name.map(name => Map(s"$name/$arity" -> TypingScheme(Map.empty, rec: ErlType[Plus]))).getOrElse(Map.empty)
    clauses.foldLeft((ext, TypingScheme(Map.empty, rec: ErlType[Plus]))) {
      case ((env, _), clause) =>
        val TypingScheme(delta, typ) = clause.check_+(env)
        val scheme = TypingScheme(delta, rec: ErlType[Plus]).inst(typ, inv)
        (name.fold(env)(name => env.updated(s"$name/$arity", scheme)), scheme)
    }._2
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunRefTree(name: String, arity: Int) extends ErlTree {
  def check_+(env: Pi) = env(name)
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunCallTree(fun: ErlTree, args: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (f, types)) = for {
      types <- checkAll(env, args)
      f <- fun.check_+(env)
      f <- f match {
        case AtomType(name) => env(s"$name/${args.size}")
        case _ => TypingScheme(Map.empty, f)
      }
    } yield (f, types)
    val v = VarType[Plus](ErlType.fresh)
    TypingScheme[ErlType[Plus]](delta, v).inst(f, FunctionType[Minus](types, VarType(v.id)))
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class AssignTree(lhs: ErlTree, rhs: ErlTree) extends ErlTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, (l, r)) = for {
      r <- rhs.check_+(env)
      l <- lhs.check_+(env)
    } yield (l, r)
    TypingScheme(delta, l).inst(r, lhs.check_-(delta).typ)
  }
  def check_-(env: Delta) = {
    rhs.check_-(env) match {
      case TypingScheme(delta, r) =>
        lhs.check_-(delta) match {
          case TypingScheme(delta, l) =>
            TypingScheme(delta, l).inst(lhs.check_+(Map.empty).typ, r)
        }
    }
  }
}

object ErlTree {

  def alpha(tree: ErlTree): ErlTree = ???

}
