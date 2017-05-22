package erltype

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap

class ParseListener extends ErlangBaseListener { self =>

  private[this] val result: Buffer[Tree[Pos]] = Buffer.empty

  private[this] val table: HashMap[String, Long] = HashMap.empty

  private[this] lazy val pos: PosAnalyzer = new PosAnalyzer {
    val table = self.table
    val neg = self.neg
  }

  private[this] lazy val neg: NegAnalyzer = new NegAnalyzer {
    val table = self.table
  }

  override def exitFunction(ctx: ErlangParser.FunctionContext): Unit = {
    val fun = pos.fromFunction(ctx)
    println(fun.show)
    result += fun
  }

  def getResult: Vector[Tree[Pos]] = result.toVector

}

trait Analyzer[A <: Polarity] {

  def fromExpr(ctx: ErlangParser.ExprContext)(k: Tree[A] => Tree[A]): Tree[A]

  def fromAtomic(atomic: ErlangParser.AtomicContext): Tree[A] =
    Option(atomic.tokChar).map(tokChar => CharTree[A](tokChar.getText.charAt(0))) orElse
    Option(atomic.tokInteger).map(tokInteger => IntTree[A](tokInteger.getText.toInt)) orElse
    Option(atomic.tokFloat).map(tokFloat => FloatTree[A](tokFloat.getText.toFloat)) orElse
    Option(atomic.tokAtom).map(tokAtom => AtomTree[A](tokAtom.getText)) orElse
    Option(atomic.tokString).map(tokString => StringTree[A](tokString.asScala.map(_.getText).mkString)) getOrElse (throw new RuntimeException)

  def fromTuple(ctx: ErlangParser.TupleContext)(k: Tree[A] => Tree[A]): Tree[A] = {
    def makeTuple(exprs: List[ErlangParser.ExprContext])(k: List[Tree[A]] => Tree[A]): Tree[A] =
      exprs match {
        case Nil => k(Nil)
        case expr :: exprs => fromExpr(expr)(e => makeTuple(exprs)(es => k(e :: es)))
      }
    makeTuple(Option(ctx.exprs).toList.flatMap(_.expr.asScala))(es => k(TupleTree(es)))
  }

  def fromList(ctx: ErlangParser.ListContext)(k: Tree[A] => Tree[A]): Tree[A] =
    Option(ctx.expr).map { expr =>
      fromExpr(expr)(e => fromTail(e, ctx.tail)((es, t) => k(ListTree(es, t))))
    }.getOrElse {
      k(ListTree(Nil, None))
    }

  def fromTail(head: Tree[A], tail: ErlangParser.TailContext)(k: (List[Tree[A]], Option[Tree[A]]) => Tree[A]): Tree[A] =
    Option(tail.expr).map { expr =>
      Option(tail.tail).map { tail =>
        fromExpr(expr)(e => fromTail(e, tail)((es, t) => k(head :: e :: es, t)))
      }.getOrElse {
        fromExpr(expr)(e => k(List(head), Some(e)))
      }
    }.getOrElse {
      k(List(head), None)
    }

}

trait PosAnalyzer extends Analyzer[Pos] {

  def table: HashMap[String, Long]

  def neg: NegAnalyzer

  def fromExpr(ctx: ErlangParser.ExprContext)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    fromExpr100(ctx.expr100)(k)

  def fromExpr100(ctx: ErlangParser.Expr100Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr150.asScala
    matchToFunCall(expr, exprs)(k)
  }

  def matchToFunCall(lhs: ErlangParser.Expr150Context, rhs: Seq[ErlangParser.Expr150Context])(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    rhs match {
      case Seq() => fromExpr150(lhs)(k)
      case r +: rs =>
        matchToFunCall(r, rs) { r =>
          fromExpr150(lhs) { l =>
            FunCallTree(FunTree(List(FunClauseTree(None, List(neg.fromExpr150(lhs)(identity)), IfClauseTree(Nil, k(l))))), List(r))
          }
        }
    }

  def fromExpr150(ctx: ErlangParser.Expr150Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr160.asScala
    fromExpr160(expr)(e => makeBinOpCall(e, exprs zip Seq.fill(exprs.size)("orelse"))(fromExpr160)(k))
  }

  def fromExpr160(ctx: ErlangParser.Expr160Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr200.asScala
    fromExpr200(expr)(e => makeBinOpCall(e, exprs zip Seq.fill(exprs.size)("andalso"))(fromExpr200)(k))
  }

  def fromExpr200(ctx: ErlangParser.Expr200Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr300.asScala
    fromExpr300(expr)(e => makeBinOpCall(e, exprs zip Option(ctx.compOp).map(_.getText).toList)(fromExpr300)(k))
  }

  def fromExpr300(ctx: ErlangParser.Expr300Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr400.asScala
    fromExpr400(expr)(e => makeBinOpCall(e, exprs zip ctx.listOp.asScala.map(_.getText))(fromExpr400)(k))
  }

  def fromExpr400(ctx: ErlangParser.Expr400Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr500.asScala
    fromExpr500(expr)(e => makeBinOpCall(e, exprs zip ctx.addOp.asScala.map(_.getText))(fromExpr500)(k))
  }

  def fromExpr500(ctx: ErlangParser.Expr500Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    val expr +: exprs = ctx.expr600.asScala
    fromExpr600(expr)(e => makeBinOpCall(e, exprs zip ctx.multOp.asScala.map(_.getText))(fromExpr600)(k))
  }

  def fromExpr600(ctx: ErlangParser.Expr600Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    Option(ctx.prefixOp).map(prefixOp => fromExpr700(ctx.expr700)(e => k(FunCallTree(FunRefTree(prefixOp.getText, 1), List(e))))) getOrElse
    fromExpr700(ctx.expr700)(k)

  def fromExpr700(expr: ErlangParser.Expr700Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    Option(expr.expr800).map(fromExpr800(_)(k)) orElse
    Option(expr.functionCall).map(fromFunctionCall(_)(k))getOrElse (throw new RuntimeException)

  def fromExpr800(expr: ErlangParser.Expr800Context)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = fromExprMax(expr.exprMax.get(0))(k)

  def fromExprMax(expr: ErlangParser.ExprMaxContext)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    Option(expr.tokVar).map(tokVar => k(VarTree[Pos](table.getOrElseUpdate(tokVar.getText, fresh)))) orElse
    Option(expr.atomic).map(atomic => k(fromAtomic(atomic))) orElse
    Option(expr.expr).map(fromExpr(_)(k)) orElse
    Option(expr.funExpr).map(fromFunExpr) orElse
    Option(expr.ifExpr).map(fromIfExpr) orElse
    Option(expr.tuple).map(fromTuple(_)(k)) orElse
    Option(expr.list).map(fromList(_)(k)) getOrElse (throw new RuntimeException)

  def fromIfExpr(expr: ErlangParser.IfExprContext): Tree[Pos] =
    IfTree(
      expr.ifClauses.ifClause.asScala.map(clause => makeIfClause(Some(clause.guard), clause.clauseBody))(collection.breakOut)
    )

  def fromFunExpr(fun: ErlangParser.FunExprContext): Tree[Pos] =
    Option(fun.funClauses).map(funClauses => FunTree(funClauses.funClause.asScala.map(clause => makeFunClause(None, clause.argumentList, clause.clauseGuard, clause.clauseBody))(collection.breakOut))) orElse
    Option(fun.tokAtom).map(tokAtom => FunRefTree(tokAtom.getText, fun.tokInteger.getText.toInt)) getOrElse (throw new RuntimeException)

  def fromFunctionCall(funCall: ErlangParser.FunctionCallContext)(k: Tree[Pos] => Tree[Pos]): Tree[Pos] = {
    makeBlock(Option(funCall.argumentList.exprs).toList.flatMap(_.expr.asScala)) { args =>
      fromExpr800(funCall.expr800) { e =>
        val f = e match {
          case AtomTree(name) => FunRefTree(name, args.size)
          case expr => expr
        }
        k(FunCallTree(f, args))
      }
    }
  }

  def makeFunClause(name: Option[String], args: ErlangParser.ArgumentListContext, guards: ErlangParser.ClauseGuardContext, body: ErlangParser.ClauseBodyContext): FunClauseTree =
    FunClauseTree(
      name,
      Option(args.exprs).toList.flatMap(_.expr.asScala.map(expr => neg.fromExpr(expr)(identity))),
      makeIfClause(Option(guards.guard), body)
    )

  def fromFunction(ctx: ErlangParser.FunctionContext): FunTree = {
    val clauses = ctx.functionClause.asScala
    FunTree(clauses.map { clause =>
      table.clear()
      makeFunClause(Some(clause.tokAtom.getText), clause.clauseArgs.argumentList, clause.clauseGuard, clause.clauseBody)
    }(collection.breakOut))
  }

  def makeIfClause(guard: Option[ErlangParser.GuardContext], body: ErlangParser.ClauseBodyContext): IfClauseTree =
    IfClauseTree(
      guard.toList.flatMap(_.exprs.asScala.flatMap(_.expr.asScala.map(fromExpr(_)(identity))(collection.breakOut))),
      makeBlock(body.exprs.expr.asScala)(BlockTree(_))
    )

  def makeBlock(exprs: Seq[ErlangParser.ExprContext])(k: List[Tree[Pos]] => Tree[Pos]): Tree[Pos] =
    exprs match {
      case Seq() => k(Nil)
      case expr +: exprs => fromExpr(expr)(e => makeBlock(exprs)(es => k(e :: es)))
    }

  def makeBinOpCall[A](lhs: Tree[Pos], exprs: Seq[(A, String)])(from: A => (Tree[Pos] => Tree[Pos]) => Tree[Pos])(k: Tree[Pos] => Tree[Pos]): Tree[Pos] =
    exprs match {
      case Seq() => k(lhs)
      case (expr, op) +: exprs => from(expr)(rhs => makeBinOpCall(FunCallTree(FunRefTree(op, 2), List(lhs, rhs)), exprs)(from)(k))
    }

}

trait NegAnalyzer extends Analyzer[Neg] {

  def table: HashMap[String, Long]

  def fromExpr(ctx: ErlangParser.ExprContext)(k: Tree[Neg] => Tree[Neg]): Tree[Neg] = fromExpr100(ctx.expr100)(k)

  def fromExpr100(ctx: ErlangParser.Expr100Context)(k: Tree[Neg] => Tree[Neg]): Tree[Neg] = {
    val exprs = ctx.expr150.asScala
    k(exprs.map(fromExpr150(_)(identity)).reduceRight(MatchTree(_, _)))
  }

  def fromExpr150(ctx: ErlangParser.Expr150Context)(k: Tree[Neg] => Tree[Neg]): Tree[Neg] =
    fromExprMax(ctx.expr160.get(0)
      .expr200.get(0)
      .expr300.get(0)
      .expr400.get(0)
      .expr500.get(0)
      .expr600.get(0)
      .expr700
      .expr800
      .exprMax.get(0))(k)

  def fromExprMax(expr: ErlangParser.ExprMaxContext)(k: Tree[Neg] => Tree[Neg]): Tree[Neg] =
    Option(expr.tokVar).map(tokVar => VarTree[Neg](table.getOrElseUpdate(tokVar.getText, fresh))) orElse
    Option(expr.atomic).map(fromAtomic) orElse
    Option(expr.tuple).map(fromTuple(_)(k)) orElse
    Option(expr.list).map(fromList(_)(k)) getOrElse (throw new RuntimeException)

}
