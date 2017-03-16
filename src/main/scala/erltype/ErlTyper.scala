package erltype

import scala.collection.JavaConverters._

case class TypingScheme[A](env: Delta, typ: A) {
  def map[B](f: A => B): TypingScheme[B] = flatMap(a => TypingScheme(Map.empty, f(a)))
  def flatMap[B](f: A => TypingScheme[B]): TypingScheme[B] = {
    val scheme = f(typ)
    val delta = env.foldLeft(scheme.env) {
      case (env, (name, typ)) => env.updated(name, env.get(name).fold(typ)(typ /\ _))
    }
    TypingScheme(delta, scheme.typ)
  }
  override def toString = {
    val delta = env.map { case (k, v) => s"$k: $v" }.mkString(", ")
    s"[$delta]$typ"
  }
}

object TypingScheme {

  implicit class PlusOp(val scheme: TypingScheme[ErlType[Plus]]) {
    def inst(x: ErlType[Plus], y: ErlType[Minus]): TypingScheme[ErlType[Plus]] = {
      val subst = ErlType.biunify(x, y)
      TypingScheme(scheme.env.mapValues(subst(_)), subst(scheme.typ))
    }
    def simplify: TypingScheme[ErlType[Plus]] = TypingScheme(scheme.env.mapValues(ErlType.simplify(_)), ErlType.simplify(scheme.typ))
  }

}

trait ErlTyper_+[A] {
  def check_+(env: Pi, ctx: A): TypingScheme[ErlType[Plus]]
}

object ErlTyper_+ {

  def checkAll[A: ErlTyper_+](env: Pi, ctx: Seq[A]): TypingScheme[List[ErlType[Plus]]] = {
    ctx.foldRight(TypingScheme(Map.empty, List.empty[ErlType[Plus]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check_+(env)
      } yield typ :: types
    }
  }

  def list[A](env: Pi, a: A)(head: A => ErlangParser.ExprContext)(tail: A => ErlangParser.TailContext): TypingScheme[ErlType[Plus]] = {
    Option(head(a)).map { head =>
      for {
        h <- head.check_+(env)
        t <- list(env, tail(a))(_.expr)(_.tail)
      } yield h \/ t
    }.getOrElse(TypingScheme(Map.empty, ErlType.Bottom))
  }

  def function(env: Pi, argumentList: ErlangParser.ArgumentListContext, body: ErlangParser.ClauseBodyContext): TypingScheme[ErlType[Plus]] = {
    val TypingScheme(delta, ret) = checkAll(env, body.exprs.expr.asScala).map(_.lastOption.getOrElse(ErlType.Bottom))
    Option(argumentList.exprs).toList.flatMap(_.expr.asScala).foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(ErlFunction[Plus](_, ret))
  }

  implicit def TokVarContext: ErlTyper_+[ErlangParser.TokVarContext] =
    new ErlTyper_+[ErlangParser.TokVarContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokVarContext) = {
        val name = ctx.getText
        val v = ErlType.fresh[Plus]
        env.getOrElse(name, TypingScheme(Map(name -> ErlVar(Polarity.Minus, v.id)), v))
      }
    }

  implicit def TokCharContext: ErlTyper_+[ErlangParser.TokCharContext] =
    new ErlTyper_+[ErlangParser.TokCharContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokCharContext) = TypingScheme(Map.empty, ErlChar())
    }

  implicit def TokIntegerContext: ErlTyper_+[ErlangParser.TokIntegerContext] =
    new ErlTyper_+[ErlangParser.TokIntegerContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokIntegerContext) = TypingScheme(Map.empty, ErlInteger())
    }

  implicit def TokFloatContext: ErlTyper_+[ErlangParser.TokFloatContext] =
    new ErlTyper_+[ErlangParser.TokFloatContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokFloatContext) = TypingScheme(Map.empty, ErlFloat())
    }

  implicit def TokAtomContext: ErlTyper_+[ErlangParser.TokAtomContext] =
    new ErlTyper_+[ErlangParser.TokAtomContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokAtomContext) = {
        val name = ctx.getText
        env.getOrElse(name, TypingScheme(Map.empty, ErlAtom(name)))
      }
    }

  implicit def TokStringsContext: ErlTyper_+[java.util.List[ErlangParser.TokStringContext]] =
    new ErlTyper_+[java.util.List[ErlangParser.TokStringContext]] {
      def check_+(env: Pi, ctx: java.util.List[ErlangParser.TokStringContext]) = TypingScheme(Map.empty, ErlString())
    }

  implicit def AtomicContext: ErlTyper_+[ErlangParser.AtomicContext] =
    new ErlTyper_+[ErlangParser.AtomicContext] {
      def check_+(env: Pi, ctx: ErlangParser.AtomicContext) =
        Option(ctx.tokChar).map(_.check_+(env)) orElse
        Option(ctx.tokInteger).map(_.check_+(env)) orElse
        Option(ctx.tokFloat).map(_.check_+(env)) orElse
        Option(ctx.tokAtom).map(_.check_+(env)) orElse
        Option(ctx.tokString).map(_.check_+(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionCallContext: ErlTyper_+[ErlangParser.FunctionCallContext] =
    new ErlTyper_+[ErlangParser.FunctionCallContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunctionCallContext) = {
        val args = Option(ctx.argumentList.exprs).toList.flatMap(_.expr.asScala)
        val TypingScheme(delta, (f, types)) = for {
          types <- checkAll(env, args)
          f <- ctx.expr800.check_+(env)
        } yield (f, types)
        val v = ErlType.fresh[Plus]
        TypingScheme[ErlType[Plus]](delta, v).inst(f, ErlFunction[Minus](types, ErlVar(Polarity.Minus, v.id)))
      }
    }

  implicit def Expr800Context: ErlTyper_+[ErlangParser.Expr800Context] =
    new ErlTyper_+[ErlangParser.Expr800Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr800Context) = ctx.exprMax.asScala.head.check_+(env)
    }

  implicit def Expr700Context: ErlTyper_+[ErlangParser.Expr700Context] =
    new ErlTyper_+[ErlangParser.Expr700Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr700Context) =
        Option(ctx.expr800).map(_.check_+(env)) orElse
        Option(ctx.functionCall).map(_.check_+(env)) getOrElse (throw new RuntimeException)
    }

  implicit def Expr600Context: ErlTyper_+[ErlangParser.Expr600Context] =
    new ErlTyper_+[ErlangParser.Expr600Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr600Context) = ctx.expr700.check_+(env)
    }

  implicit def Expr500Context: ErlTyper_+[ErlangParser.Expr500Context] =
    new ErlTyper_+[ErlangParser.Expr500Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr500Context) = ctx.expr600.asScala.head.check_+(env)
    }

  implicit def Expr400Context: ErlTyper_+[ErlangParser.Expr400Context] =
    new ErlTyper_+[ErlangParser.Expr400Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr400Context) = ctx.expr500.asScala.head.check_+(env)
    }

  implicit def Expr300Context: ErlTyper_+[ErlangParser.Expr300Context] =
    new ErlTyper_+[ErlangParser.Expr300Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr300Context) = ctx.expr400.asScala.head.check_+(env)
    }

  implicit def Expr200Context: ErlTyper_+[ErlangParser.Expr200Context] =
    new ErlTyper_+[ErlangParser.Expr200Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr200Context) = ctx.expr300.asScala.head.check_+(env)
    }

  implicit def Expr160Context: ErlTyper_+[ErlangParser.Expr160Context] =
    new ErlTyper_+[ErlangParser.Expr160Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr160Context) = ctx.expr200.asScala.head.check_+(env)
    }

  implicit def Expr150Context: ErlTyper_+[ErlangParser.Expr150Context] =
    new ErlTyper_+[ErlangParser.Expr150Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr150Context) = ctx.expr160.asScala.head.check_+(env)
    }

  implicit def Expr100Context: ErlTyper_+[ErlangParser.Expr100Context] =
    new ErlTyper_+[ErlangParser.Expr100Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr100Context) = ctx.expr150.asScala.head.check_+(env)
    }

  implicit def ExprContext: ErlTyper_+[ErlangParser.ExprContext] =
    new ErlTyper_+[ErlangParser.ExprContext] {
      def check_+(env: Pi, ctx: ErlangParser.ExprContext) = ctx.expr100.check_+(env)
    }

  implicit def FunClausesContext: ErlTyper_+[ErlangParser.FunClausesContext] =
    new ErlTyper_+[ErlangParser.FunClausesContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunClausesContext) = {
        ctx.funClause.asScala.foldLeft(TypingScheme(Map.empty, ErlType.Bottom)) { (scheme, funClause) =>
          for {
            x <- scheme
            y <- function(env, funClause.argumentList, funClause.clauseBody)
          } yield x \/ y
        }
      }
    }

  implicit def FunExprContext: ErlTyper_+[ErlangParser.FunExprContext] =
    new ErlTyper_+[ErlangParser.FunExprContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunExprContext) = ctx.funClauses.check_+(env)
    }

  implicit def ListContext: ErlTyper_+[ErlangParser.ListContext] =
    new ErlTyper_+[ErlangParser.ListContext] {
      def check_+(env: Pi, ctx: ErlangParser.ListContext) = list(env, ctx)(_.expr)(_.tail).map(ErlList(_))
    }

  implicit def ListComprehensionContext: ErlTyper_+[ErlangParser.ListComprehensionContext] =
    new ErlTyper_+[ErlangParser.ListComprehensionContext] {
      def check_+(env: Pi, ctx: ErlangParser.ListComprehensionContext) = ???
    }

  implicit def ExprMaxContext: ErlTyper_+[ErlangParser.ExprMaxContext] =
    new ErlTyper_+[ErlangParser.ExprMaxContext] {
      def check_+(env: Pi, ctx: ErlangParser.ExprMaxContext) =
        Option(ctx.tokVar).map(_.check_+(env)) orElse
        Option(ctx.atomic).map(_.check_+(env)) orElse
        Option(ctx.expr).map(_.check_+(env)) orElse
        Option(ctx.funExpr).map(_.check_+(env)) orElse
        Option(ctx.list).map(_.check_+(env)) orElse
        Option(ctx.listComprehension).map(_.check_+(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionContext: ErlTyper_+[ErlangParser.FunctionContext] =
    new ErlTyper_+[ErlangParser.FunctionContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunctionContext) = {
        ctx.functionClause.asScala.foldLeft(TypingScheme(Map.empty, ErlType.Bottom)) { (scheme, functionClause) =>
          for {
            x <- scheme
            y <- function(env, functionClause.clauseArgs.argumentList, functionClause.clauseBody)
          } yield x \/ y
        }
      }
    }

}

trait ErlTyper_-[A] {
  def check_-(env: Delta, ctx: A): TypingScheme[ErlType[Minus]]
}

object ErlTyper_- {

  def list[A](env: Delta, a: A)(head: A => ErlangParser.ExprContext)(tail: A => ErlangParser.TailContext): TypingScheme[ErlType[Minus]] = {
    Option(head(a)).map { head =>
      for {
        h <- head.check_-(env)
        t <- list(env, tail(a))(_.expr)(_.tail)
      } yield h /\ t
    }.getOrElse(TypingScheme(Map.empty, ErlType.Top))
  }

  implicit def TokVarContext: ErlTyper_-[ErlangParser.TokVarContext] =
    new ErlTyper_-[ErlangParser.TokVarContext] {
      def check_-(env: Delta, ctx: ErlangParser.TokVarContext) = {
        val name = ctx.getText
        TypingScheme(env - name, env.getOrElse(name, ErlType.Top))
      }
    }

  implicit def TokCharContext: ErlTyper_-[ErlangParser.TokCharContext] =
    new ErlTyper_-[ErlangParser.TokCharContext] {
      def check_-(env: Delta, ctx: ErlangParser.TokCharContext) = TypingScheme(env, ErlChar())
    }

  implicit def TokIntegerContext: ErlTyper_-[ErlangParser.TokIntegerContext] =
    new ErlTyper_-[ErlangParser.TokIntegerContext] {
      def check_-(env: Delta, ctx: ErlangParser.TokIntegerContext) = TypingScheme(env, ErlInteger())
    }

  implicit def TokFloatContext: ErlTyper_-[ErlangParser.TokFloatContext] =
    new ErlTyper_-[ErlangParser.TokFloatContext] {
      def check_-(env: Delta, ctx: ErlangParser.TokFloatContext) = TypingScheme(env, ErlFloat())
    }

  implicit def TokAtomContext: ErlTyper_-[ErlangParser.TokAtomContext] =
    new ErlTyper_-[ErlangParser.TokAtomContext] {
      def check_-(env: Delta, ctx: ErlangParser.TokAtomContext) = TypingScheme(env, ErlAtom(ctx.getText))
    }

  implicit def TokStringsContext: ErlTyper_-[java.util.List[ErlangParser.TokStringContext]] =
    new ErlTyper_-[java.util.List[ErlangParser.TokStringContext]] {
      def check_-(env: Delta, ctx: java.util.List[ErlangParser.TokStringContext]) = TypingScheme(env, ErlString())
    }

  implicit def AtomicContext: ErlTyper_-[ErlangParser.AtomicContext] =
    new ErlTyper_-[ErlangParser.AtomicContext] {
      def check_-(env: Delta, ctx: ErlangParser.AtomicContext) =
        Option(ctx.tokChar).map(_.check_-(env)) orElse
        Option(ctx.tokInteger).map(_.check_-(env)) orElse
        Option(ctx.tokFloat).map(_.check_-(env)) orElse
        Option(ctx.tokAtom).map(_.check_-(env)) orElse
        Option(ctx.tokString).map(_.check_-(env)) getOrElse (throw new RuntimeException)
    }

  implicit def ExprMaxContext: ErlTyper_-[ErlangParser.ExprMaxContext] =
    new ErlTyper_-[ErlangParser.ExprMaxContext] {
      def check_-(env: Delta, ctx: ErlangParser.ExprMaxContext) =
        Option(ctx.tokVar).map(_.check_-(env)) orElse
        Option(ctx.atomic).map(_.check_-(env)) orElse
        Option(ctx.list).map(_.check_-(env)) getOrElse (throw new RuntimeException)
    }

  implicit def ExprContext: ErlTyper_-[ErlangParser.ExprContext] =
    new ErlTyper_-[ErlangParser.ExprContext] {
      def check_-(env: Delta, ctx: ErlangParser.ExprContext) =
        ctx.expr100.
          expr150.get(0).
          expr160.get(0).
          expr200.get(0).
          expr300.get(0).
          expr400.get(0).
          expr500.get(0).
          expr600.get(0).
          expr700.
          expr800.
          exprMax.get(0).
          check_-(env)
    }

  implicit def ListContext: ErlTyper_-[ErlangParser.ListContext] =
    new ErlTyper_-[ErlangParser.ListContext] {
      def check_-(env: Delta, ctx: ErlangParser.ListContext) = list(env, ctx)(_.expr)(_.tail).map(ErlList(_))
    }

}
