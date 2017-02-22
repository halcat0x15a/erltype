package erltype

import scala.collection.JavaConverters._

trait ErlTypers {

  implicit def TokVarContext: ErlTyper[ErlangParser.TokVarContext] =
    new ErlTyper[ErlangParser.TokVarContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.TokVarContext) = {
        val name = ctx.getText
        val v = ErlType.fresh[Plus]
        env.getOrElse(name, TypingScheme(Map(name -> ErlVar(v.id)), v))
      }
    }

  implicit def TokCharContext: ErlTyper[ErlangParser.TokCharContext] =
    new ErlTyper[ErlangParser.TokCharContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.TokCharContext) = TypingScheme(Map.empty, ErlChar())
    }

  implicit def TokIntegerContext: ErlTyper[ErlangParser.TokIntegerContext] =
    new ErlTyper[ErlangParser.TokIntegerContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.TokIntegerContext) = TypingScheme(Map.empty, ErlInteger())
    }

  implicit def TokFloatContext: ErlTyper[ErlangParser.TokFloatContext] =
    new ErlTyper[ErlangParser.TokFloatContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.TokFloatContext) = TypingScheme(Map.empty, ErlFloat())
    }

  implicit def TokAtomContext: ErlTyper[ErlangParser.TokAtomContext] =
    new ErlTyper[ErlangParser.TokAtomContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.TokAtomContext) = TypingScheme(Map.empty, ErlAtom(ctx.getText))
    }

  implicit def TokStringsContext: ErlTyper[java.util.List[ErlangParser.TokStringContext]] =
    new ErlTyper[java.util.List[ErlangParser.TokStringContext]] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: java.util.List[ErlangParser.TokStringContext]) = TypingScheme(Map.empty, ErlString())
    }

  implicit def AtomicContext: ErlTyper[ErlangParser.AtomicContext] =
    new ErlTyper[ErlangParser.AtomicContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.AtomicContext) =
        Option(ctx.tokChar).map(_.check(env)) orElse
        Option(ctx.tokInteger).map(_.check(env)) orElse
        Option(ctx.tokFloat).map(_.check(env)) orElse
        Option(ctx.tokAtom).map(_.check(env)) orElse
        Option(ctx.tokString).map(_.check(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionCallContext: ErlTyper[ErlangParser.FunctionCallContext] =
    new ErlTyper[ErlangParser.FunctionCallContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.FunctionCallContext) = {
        val params = Option(ctx.argumentList.exprs).toList.flatMap(_.expr.asScala)
        val TypingScheme(delta, (v, p, m)) = for {
          types <- params.foldRight(TypingScheme(Map.empty, List.empty[ErlType[Plus]])) { (param, scheme) =>
            for {
              types <- scheme
              typ <- param.check(env)
            } yield typ :: types
          }
          body <- ctx.expr800.check(env)
        } yield {
          val v = ErlType.fresh[Plus]
          (v, body, ErlFunction[Minus](types, ErlVar(v.id)))
        }
        ErlTyper.inst(TypingScheme(delta, v), p, m)
      }
    }

  implicit def Expr800Context: ErlTyper[ErlangParser.Expr800Context] =
    new ErlTyper[ErlangParser.Expr800Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr800Context) = ctx.exprMax.asScala.head.check(env)
    }

  implicit def Expr700Context: ErlTyper[ErlangParser.Expr700Context] =
    new ErlTyper[ErlangParser.Expr700Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr700Context) =
        Option(ctx.expr800).map(_.check(env)) orElse
        Option(ctx.functionCall).map(_.check(env)) getOrElse (throw new RuntimeException)
    }

  implicit def Expr600Context: ErlTyper[ErlangParser.Expr600Context] =
    new ErlTyper[ErlangParser.Expr600Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr600Context) = ctx.expr700.check(env)
    }

  implicit def Expr500Context: ErlTyper[ErlangParser.Expr500Context] =
    new ErlTyper[ErlangParser.Expr500Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr500Context) = ctx.expr600.asScala.head.check(env)
    }

  implicit def Expr400Context: ErlTyper[ErlangParser.Expr400Context] =
    new ErlTyper[ErlangParser.Expr400Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr400Context) = ctx.expr500.asScala.head.check(env)
    }

  implicit def Expr300Context: ErlTyper[ErlangParser.Expr300Context] =
    new ErlTyper[ErlangParser.Expr300Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr300Context) = ctx.expr400.asScala.head.check(env)
    }

  implicit def Expr200Context: ErlTyper[ErlangParser.Expr200Context] =
    new ErlTyper[ErlangParser.Expr200Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr200Context) = ctx.expr300.asScala.head.check(env)
    }

  implicit def Expr160Context: ErlTyper[ErlangParser.Expr160Context] =
    new ErlTyper[ErlangParser.Expr160Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr160Context) = ctx.expr200.asScala.head.check(env)
    }

  implicit def Expr150Context: ErlTyper[ErlangParser.Expr150Context] =
    new ErlTyper[ErlangParser.Expr150Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr150Context) = ctx.expr160.asScala.head.check(env)
    }

  implicit def Expr100Context: ErlTyper[ErlangParser.Expr100Context] =
    new ErlTyper[ErlangParser.Expr100Context] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.Expr100Context) = ctx.expr150.asScala.head.check(env)
    }

  implicit def ExprContext: ErlTyper[ErlangParser.ExprContext] =
    new ErlTyper[ErlangParser.ExprContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.ExprContext) = ctx.expr100.check(env)
    }

  implicit def FunClausesContext: ErlTyper[ErlangParser.FunClausesContext] =
    new ErlTyper[ErlangParser.FunClausesContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.FunClausesContext) = {
        val funClause = ctx.funClause.asScala.head
        val params = Option(funClause.argumentList.exprs).toList.flatMap(_.expr.asScala).map(_.getText) // TODO
        // funClause.clauseGuard
        val body = funClause.clauseBody.exprs.expr.asScala.head.check(env)
        TypingScheme(body.env -- params, ErlFunction(params.map(body.env.getOrElse(_, ErlIntersection(Nil))), body.typ))
      }
    }

  implicit def FunExprContext: ErlTyper[ErlangParser.FunExprContext] =
    new ErlTyper[ErlangParser.FunExprContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.FunExprContext) = ctx.funClauses.check(env)
    }

  implicit def ListContext: ErlTyper[ErlangParser.ListContext] =
    new ErlTyper[ErlangParser.ListContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.ListContext) = ???
    }

  implicit def ListComprehensionContext: ErlTyper[ErlangParser.ListComprehensionContext] =
    new ErlTyper[ErlangParser.ListComprehensionContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.ListComprehensionContext) = ???
    }

  implicit def ExprMaxContext: ErlTyper[ErlangParser.ExprMaxContext] =
    new ErlTyper[ErlangParser.ExprMaxContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.ExprMaxContext) =
        Option(ctx.tokVar).map(_.check(env)) orElse
        Option(ctx.atomic).map(_.check(env)) orElse
        Option(ctx.expr).map(_.check(env)) orElse
        Option(ctx.funExpr).map(_.check(env)) orElse
        Option(ctx.list).map(_.check(env)) orElse
        Option(ctx.listComprehension).map(_.check(env)) getOrElse (throw new RuntimeException)
    }

  implicit def FunctionClauseContext: ErlTyper[ErlangParser.FunctionClauseContext] =
    new ErlTyper[ErlangParser.FunctionClauseContext] {
      def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: ErlangParser.FunctionClauseContext) = {
        val funClause = ctx
        val params = Option(funClause.clauseArgs.argumentList.exprs).toList.flatMap(_.expr.asScala)
        // funClause.clauseGuard
        val body = funClause.clauseBody.exprs.expr.asScala.head
        for {
          types <- params.foldRight(TypingScheme(Map.empty, List.empty[ErlType[Plus]])) { (param, scheme) =>
            for {
              types <- scheme
              typ <- param.check(env)
            } yield typ :: types
          }
          body <- body.check(env)
        } yield ErlFunction[Plus](types.asInstanceOf[List[ErlType[Minus]]], body) // TODO
      }
    }

}
