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

case class VarTree(value: String) extends ErlTree {
  def check_+(env: Pi) = {
    val v = ErlType.fresh[Plus]
    env.getOrElse(value, TypingScheme(Map(value -> VarType(v.id)), v))
  }
  def check_-(env: Delta) = {
    TypingScheme(env - value, env.getOrElse(value, ErlType.Top))
  }
}

sealed abstract class ListTree extends ErlTree

case object NilTree extends ListTree {
  def check_+(env: Pi) = TypingScheme(Map.empty, ListType(ErlType.Bottom))
  def check_-(env: Delta) = TypingScheme(env, ListType(ErlType.Top))
}

case class ConsTree(head: ErlTree, tail: ListTree) extends ListTree {
  def check_+(env: Pi) = for {
    h <- head.check_+(env)
    t <- tail.check_+(env)
  } yield {
    val ListType(typ) = t
    ListType(h \/ typ)
  }
  def check_-(env: Delta) = {
    head.check_-(env) match {
      case TypingScheme(env, h) =>
        tail.check_-(env) match {
          case TypingScheme(env, t) =>
            val ListType(typ) = t
            TypingScheme(env, ListType(h /\ typ): ErlType[Minus])
        }
    }
  }
}

case class TailTree(tree: ErlTree) extends ListTree {
  def check_+(env: Pi) = {
    val TypingScheme(delta, tail) = tree.check_+(env)
    val v = ErlType.fresh[Plus]
    TypingScheme[ErlType[Plus]](delta, ListType(v)).inst(tail, ListType(VarType(v.id)))
  }
  def check_-(env: Delta) = {
    val TypingScheme(delta, tail) = tree.check_-(env)
    val v = ErlType.fresh[Minus]
    TypingScheme[ErlType[Minus]](delta, ListType(v)).inst(ListType(VarType(v.id)), tail)
  }
}

case class FunClauseTree(name: Option[String], args: List[ErlTree], guards: List[List[ErlTree]], body: List[ErlTree]) extends ErlTree {
  def check_+(env: Pi) = {
    val rec = ErlType.fresh[Plus]
    val ext = name.map(name => Map(s"$name/${args.size}" -> TypingScheme(Map.empty, rec: ErlType[Plus]))).getOrElse(Map.empty)
    val TypingScheme(delta, ret) = checkAll(env ++ ext, body).map(_.lastOption.getOrElse(ErlType.Bottom))
    val result = args.foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(FunctionType[Plus](_, ret): ErlType[Plus])
    TypingScheme(result.env, VarType(rec.id): ErlType[Plus]).inst(result.typ, VarType(rec.id))
  }
  def check_-(env: Delta) = throw new RuntimeException
}

case class FunTree(clauses: List[FunClauseTree]) extends ErlTree {
  def check_+(env: Pi) = {
    println(clauses.map(_.check_+(env)))
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
        case AtomType(name) => env(s"$name/${args.size}")
        case _ => TypingScheme(Map.empty, f)
      }
    } yield (f, types)
    val v = ErlType.fresh[Plus]
    TypingScheme[ErlType[Plus]](delta, v).inst(f, FunctionType[Minus](types, VarType(v.id)))
  }
  def check_-(env: Delta) = throw new RuntimeException
}
