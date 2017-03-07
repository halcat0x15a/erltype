package erltype

import scala.collection.JavaConverters._

trait ErlTypers {

  def checkAll[A: ErlTyper](env: Pi, ctx: Seq[A]): TypingScheme[List[ErlType[Plus]]] = {
    ctx.foldRight(TypingScheme(Map.empty, List.empty[ErlType[Plus]])) { (expr, scheme) =>
      for {
        types <- scheme
        typ <- expr.check_+(env)
      } yield typ :: types
    }
  }

  def function(env: Pi, argumentList: ErlangParser.ArgumentListContext, body: ErlangParser.ClauseBodyContext): TypingScheme[ErlType[Plus]] = {
    val TypingScheme(delta, ret) = checkAll(env, body.exprs.expr.asScala).map(_.lastOption.getOrElse(ErlType.Bottom))
    Option(argumentList.exprs).toList.flatMap(_.expr.asScala).foldRight(TypingScheme(delta, List.empty[ErlType[Minus]])) {
      case (arg, TypingScheme(env, types)) => arg.check_-(env).map(_ :: types)
    }.map(ErlFunction[Plus](_, ret))
  }

  implicit def TokVarContext: ErlTyper[ErlangParser.TokVarContext] =
    new ErlTyper[ErlangParser.TokVarContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokVarContext) = {
        val name = ctx.getText
        val v = ErlType.fresh[Plus]
        env.getOrElse(name, TypingScheme(Map(name -> ErlVar(v.id)), v))
      }
      def check_-(env: Delta, ctx: ErlangParser.TokVarContext) = {
        val name = ctx.getText
        TypingScheme(env - name, env.getOrElse(name, ErlType.Top))
      }
    }

  implicit def TokCharContext: ErlTyper[ErlangParser.TokCharContext] =
    new ErlTyper[ErlangParser.TokCharContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokCharContext) = TypingScheme(Map.empty, ErlChar())
      def check_-(env: Delta, ctx: ErlangParser.TokCharContext) = TypingScheme(env, ErlChar())
    }

  implicit def TokIntegerContext: ErlTyper[ErlangParser.TokIntegerContext] =
    new ErlTyper[ErlangParser.TokIntegerContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokIntegerContext) = TypingScheme(Map.empty, ErlInteger())
      def check_-(env: Delta, ctx: ErlangParser.TokIntegerContext) = TypingScheme(env, ErlInteger())
    }

  implicit def TokFloatContext: ErlTyper[ErlangParser.TokFloatContext] =
    new ErlTyper[ErlangParser.TokFloatContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokFloatContext) = TypingScheme(Map.empty, ErlFloat())
      def check_-(env: Delta, ctx: ErlangParser.TokFloatContext) = TypingScheme(env, ErlFloat())
    }

  implicit def TokAtomContext: ErlTyper[ErlangParser.TokAtomContext] =
    new ErlTyper[ErlangParser.TokAtomContext] {
      def check_+(env: Pi, ctx: ErlangParser.TokAtomContext) = {
        val name = ctx.getText
        env.getOrElse(name, TypingScheme(Map.empty, ErlAtom(name)))
      }
      def check_-(env: Delta, ctx: ErlangParser.TokAtomContext) = TypingScheme(env, ErlAtom(ctx.getText))
    }

  implicit def TokStringsContext: ErlTyper[java.util.List[ErlangParser.TokStringContext]] =
    new ErlTyper[java.util.List[ErlangParser.TokStringContext]] {
      def check_+(env: Pi, ctx: java.util.List[ErlangParser.TokStringContext]) = TypingScheme(Map.empty, ErlString())
      def check_-(env: Delta, ctx: java.util.List[ErlangParser.TokStringContext]) = TypingScheme(env, ErlString())
    }

  implicit def AtomicContext: ErlTyper[ErlangParser.AtomicContext] =
    new ErlTyper[ErlangParser.AtomicContext] {
      def check_+(env: Pi, ctx: ErlangParser.AtomicContext) =
        Option(ctx.tokChar).map(_.check_+(env)) orElse
        Option(ctx.tokInteger).map(_.check_+(env)) orElse
        Option(ctx.tokFloat).map(_.check_+(env)) orElse
        Option(ctx.tokAtom).map(_.check_+(env)) orElse
        Option(ctx.tokString).map(_.check_+(env)) getOrElse (throw new RuntimeException)
      def check_-(env: Delta, ctx: ErlangParser.AtomicContext) =
        Option(ctx.tokChar).map(_.check_-(env)) orElse
        Option(ctx.tokInteger).map(_.check_-(env)) orElse
        Option(ctx.tokFloat).map(_.check_-(env)) orElse
        Option(ctx.tokAtom).map(_.check_-(env)) orElse
        Option(ctx.tokString).map(_.check_-(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionCallContext: ErlTyper[ErlangParser.FunctionCallContext] =
    new ErlTyper[ErlangParser.FunctionCallContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunctionCallContext) = {
        val args = Option(ctx.argumentList.exprs).toList.flatMap(_.expr.asScala)
        val TypingScheme(delta, (f, types)) = for {
          types <- checkAll(env, args)
          f <- ctx.expr800.check_+(env)
        } yield (f, types)
        val v = ErlType.fresh[Plus]
        TypingScheme[ErlType[Plus]](delta, v).inst(f, ErlFunction[Minus](types, ErlVar(v.id)))
      }
      def check_-(env: Delta, ctx: ErlangParser.FunctionCallContext) = throw new RuntimeException
    }

  implicit def Expr800Context: ErlTyper[ErlangParser.Expr800Context] =
    new ErlTyper[ErlangParser.Expr800Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr800Context) = ctx.exprMax.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr800Context) = ctx.exprMax.asScala.head.check_-(env)
    }

  implicit def Expr700Context: ErlTyper[ErlangParser.Expr700Context] =
    new ErlTyper[ErlangParser.Expr700Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr700Context) =
        Option(ctx.expr800).map(_.check_+(env)) orElse
        Option(ctx.functionCall).map(_.check_+(env)) getOrElse (throw new RuntimeException)
      def check_-(env: Delta, ctx: ErlangParser.Expr700Context) = Option(ctx.expr800).map(_.check_-(env)) getOrElse (throw new RuntimeException)
    }

  implicit def Expr600Context: ErlTyper[ErlangParser.Expr600Context] =
    new ErlTyper[ErlangParser.Expr600Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr600Context) = ctx.expr700.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr600Context) = ctx.expr700.check_-(env)
    }

  implicit def Expr500Context: ErlTyper[ErlangParser.Expr500Context] =
    new ErlTyper[ErlangParser.Expr500Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr500Context) = ctx.expr600.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr500Context) = ctx.expr600.asScala.head.check_-(env)
    }

  implicit def Expr400Context: ErlTyper[ErlangParser.Expr400Context] =
    new ErlTyper[ErlangParser.Expr400Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr400Context) = ctx.expr500.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr400Context) = ctx.expr500.asScala.head.check_-(env)
    }

  implicit def Expr300Context: ErlTyper[ErlangParser.Expr300Context] =
    new ErlTyper[ErlangParser.Expr300Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr300Context) = ctx.expr400.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr300Context) = ctx.expr400.asScala.head.check_-(env)
    }

  implicit def Expr200Context: ErlTyper[ErlangParser.Expr200Context] =
    new ErlTyper[ErlangParser.Expr200Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr200Context) = ctx.expr300.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr200Context) = ctx.expr300.asScala.head.check_-(env)
    }

  implicit def Expr160Context: ErlTyper[ErlangParser.Expr160Context] =
    new ErlTyper[ErlangParser.Expr160Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr160Context) = ctx.expr200.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr160Context) = ctx.expr200.asScala.head.check_-(env)
    }

  implicit def Expr150Context: ErlTyper[ErlangParser.Expr150Context] =
    new ErlTyper[ErlangParser.Expr150Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr150Context) = ctx.expr160.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr150Context) = ctx.expr160.asScala.head.check_-(env)
    }

  implicit def Expr100Context: ErlTyper[ErlangParser.Expr100Context] =
    new ErlTyper[ErlangParser.Expr100Context] {
      def check_+(env: Pi, ctx: ErlangParser.Expr100Context) = ctx.expr150.asScala.head.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.Expr100Context) = ctx.expr150.asScala.head.check_-(env)
    }

  implicit def ExprContext: ErlTyper[ErlangParser.ExprContext] =
    new ErlTyper[ErlangParser.ExprContext] {
      def check_+(env: Pi, ctx: ErlangParser.ExprContext) = ctx.expr100.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.ExprContext) = ctx.expr100.check_-(env)
    }

  implicit def FunClausesContext: ErlTyper[ErlangParser.FunClausesContext] =
    new ErlTyper[ErlangParser.FunClausesContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunClausesContext) = {
        ctx.funClause.asScala.foldLeft(TypingScheme(Map.empty, ErlType.Bottom)) { (scheme, funClause) =>
          for {
            x <- scheme
            y <- function(env, funClause.argumentList, funClause.clauseBody)
          } yield x \/ y
        }
      }
      def check_-(env: Delta, ctx: ErlangParser.FunClausesContext) = throw new RuntimeException
    }

  implicit def FunExprContext: ErlTyper[ErlangParser.FunExprContext] =
    new ErlTyper[ErlangParser.FunExprContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunExprContext) = ctx.funClauses.check_+(env)
      def check_-(env: Delta, ctx: ErlangParser.FunExprContext) = throw new RuntimeException
    }

  implicit def ListContext: ErlTyper[ErlangParser.ListContext] =
    new ErlTyper[ErlangParser.ListContext] {
      def check_+(env: Pi, ctx: ErlangParser.ListContext) = ???
      def check_-(env: Delta, ctx: ErlangParser.ListContext) = ???
    }

  implicit def ListComprehensionContext: ErlTyper[ErlangParser.ListComprehensionContext] =
    new ErlTyper[ErlangParser.ListComprehensionContext] {
      def check_+(env: Pi, ctx: ErlangParser.ListComprehensionContext) = ???
      def check_-(env: Delta, ctx: ErlangParser.ListComprehensionContext) = throw new RuntimeException
    }

  implicit def ExprMaxContext: ErlTyper[ErlangParser.ExprMaxContext] =
    new ErlTyper[ErlangParser.ExprMaxContext] {
      def check_+(env: Pi, ctx: ErlangParser.ExprMaxContext) =
        Option(ctx.tokVar).map(_.check_+(env)) orElse
        Option(ctx.atomic).map(_.check_+(env)) orElse
        Option(ctx.expr).map(_.check_+(env)) orElse
        Option(ctx.funExpr).map(_.check_+(env)) orElse
        Option(ctx.list).map(_.check_+(env)) orElse
        Option(ctx.listComprehension).map(_.check_+(env)) getOrElse (throw new RuntimeException)
      def check_-(env: Delta, ctx: ErlangParser.ExprMaxContext) =
        Option(ctx.tokVar).map(_.check_-(env)) orElse
        Option(ctx.atomic).map(_.check_-(env)) orElse
        Option(ctx.expr).map(_.check_-(env)) orElse
        Option(ctx.list).map(_.check_-(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionContext: ErlTyper[ErlangParser.FunctionContext] =
    new ErlTyper[ErlangParser.FunctionContext] {
      def check_+(env: Pi, ctx: ErlangParser.FunctionContext) = {
        ctx.functionClause.asScala.foldLeft(TypingScheme(Map.empty, ErlType.Bottom)) { (scheme, functionClause) =>
          for {
            x <- scheme
            y <- function(env, functionClause.clauseArgs.argumentList, functionClause.clauseBody)
          } yield x \/ y
        }
      }
      def check_-(env: Delta, ctx: ErlangParser.FunctionContext) = throw new RuntimeException
    }

}
