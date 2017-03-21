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

sealed abstract class ListTree extends ErlTree

case object NilTree extends ListTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ErlList(ErlType.Bottom))
  def check_-(env: Delta) = TypingScheme(env, ErlList(ErlType.Top))
}

case class ConsTree(head: ErlTree, tail: ListTree) extends ListTree {
  def check_+(env: Pi) = for {
    h <- head.check_+(env)
    t <- tail.check_+(env)
  } yield {
    val ErlList(typ) = t
    ErlList(h \/ typ)
  }
  def check_-(env: Delta) = {
    head.check_-(env) match {
      case TypingScheme(env, h) =>
        tail.check_-(env) match {
          case TypingScheme(env, t) =>
            val ErlList(typ) = t
            TypingScheme(env, ErlList(h /\ typ): ErlType[Minus])
        }
    }
  }
}

case class TailTree(tree: ErlTree) extends ListTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, tail) = tree.check_+(env)
    val v = ErlType.fresh[Plus]
    TypingScheme[ErlType[Plus]](delta, ErlList(v)).inst(tail, ErlList(ErlVar(v.id)))
  }
  def check_-(env: Delta) = {
    val TypingScheme(delta, tail) = tree.check_-(env)
    val v = ErlType.fresh[Minus]
    TypingScheme[ErlType[Minus]](delta, ErlList(v)).inst(ErlList(ErlVar(v.id)), tail)
  }
}

case class FunClauseTree(name: Option[String], args: List[ErlTree], guards: List[List[ErlTree]], body: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val rec = ErlType.fresh[Plus]
    val ext = name.map(name => Map(s"$name/${args.size}" -> TypingScheme(Map.empty, rec: ErlType[Plus]))).getOrElse(Map.empty)
    val TypingScheme(delta, ret) = checkAll(env ++ ext, body).map(_.lastOption.getOrElse(ErlType.Bottom))
    val result = args.foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(ErlFunction[Plus](_, ret): ErlType[Plus])
    TypingScheme(result.env, ErlVar(rec.id): ErlType[Plus]).inst(result.typ, ErlVar(rec.id))
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunTree(clauses: List[FunClauseTree]) extends ErlTree {
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
        case ErlAtom(name) => env(s"$name/${args.size}")
        case _ => TypingScheme(Map.empty, f)
      }
    } yield (f, types)
    val v = ErlType.fresh[Plus]
    TypingScheme[ErlType[Plus]](delta, v).inst(f, ErlFunction[Minus](types, ErlVar(v.id)))
  }
  def check_-(env: Delta) = throw new RuntimeException
}
