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
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlInteger())
  def check_-(env: Delta) = TypingScheme(env, ErlInteger())
}

case class FloatTree(value: Float) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlFloat())
  def check_-(env: Delta) = TypingScheme(env, ErlFloat())
}

case class CharTree(value: Char) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlChar())
  def check_-(env: Delta) = TypingScheme(env, ErlChar())
}

case class AtomTree(value: String) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlAtom(value))
  def check_-(env: Delta) = TypingScheme(env, ErlAtom(value))
}

case class StringTree(value: String) extends ErlTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlString())
  def check_-(env: Delta) = TypingScheme(env, ErlString())
}

case class VarTree(value: String) extends ErlTree {
  def check_+(env: Pi) = {
    val v = ErlType.fresh[Plus]
    env.getOrElse(value, TypingScheme(Map(value -> ErlVar(v.id)), v))
  }
  def check_-(env: Delta) = {
    TypingScheme(env - value, env.getOrElse(value, ErlType.Top))
  }
}

case class ListTree(value: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = checkAll(env, value).map(types => ErlList(types.foldLeft(ErlType.Bottom)(_ \/ _)))
  def check_-(env: Delta) =
    value.foldRight(TypingScheme(Map.empty, ErlType.Top)) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check_-(env)
      } yield typ /\ types
    }
}

case class FunClauseTree(args: List[ErlTree], guards: List[List[ErlTree]], body: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, ret) = checkAll(env, body).map(_.lastOption.getOrElse(ErlType.Bottom))
    args.foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(ErlFunction[Plus](_, ret))
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunTree(name: Option[String], clauses: List[FunClauseTree]) extends ErlTree {
  def check_+(env: Pi) = {
    clauses.foldLeft(TypingScheme(Map.empty, ErlType.Bottom)) { (scheme, clause) =>
      for {
        x <- scheme
        y <- clause.check_+(env)
      } yield x \/ y
    }
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
        case ErlAtom(name) => env(name)
        case _ => TypingScheme(Map.empty, f)
      }
    } yield (f, types)
    val v = ErlType.fresh[Plus]
    TypingScheme[ErlType[Plus]](delta, v).inst(f, ErlFunction[Minus](types, ErlVar(v.id)))
  }
  def check_-(env: Delta) = throw new RuntimeException
}
