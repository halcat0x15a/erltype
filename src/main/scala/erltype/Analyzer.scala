package erltype

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap

class Analyzer extends ErlangBaseListener {

  private var result: Buffer[Tree[_ <: Polarity]] = Buffer.empty

  private var table: HashMap[String, Long] = HashMap.empty

  def untype[A <: Polarity](tree: Tree[_ <: Polarity]): Tree[A] = tree.asInstanceOf[Tree[A]]

  override def exitFunction(ctx: ErlangParser.FunctionContext): Unit = {
    val clauses = ctx.functionClause.asScala
    val tree = FunTree(clauses.map { clause =>
      table.clear()
      makeFunClause(Some(clause.tokAtom.getText), clause.clauseArgs.argumentList, clause.clauseGuard, clause.clauseBody)
    }(collection.breakOut))
    result += tree
  }

  def fromAtomic[A <: Polarity](atomic: ErlangParser.AtomicContext): Tree[A] =
    Option(atomic.tokChar).map(tokChar => CharTree[A](tokChar.getText.charAt(0))) orElse
    Option(atomic.tokInteger).map(tokInteger => IntTree[A](tokInteger.getText.toInt)) orElse
    Option(atomic.tokFloat).map(tokFloat => FloatTree[A](tokFloat.getText.toFloat)) orElse
    Option(atomic.tokAtom).map(tokAtom => AtomTree[A](tokAtom.getText)) orElse
    Option(atomic.tokString).map(tokString => StringTree[A](tokString.asScala.map(_.getText).mkString)) getOrElse (throw new RuntimeException)

  def fromExprMax(expr: ErlangParser.ExprMaxContext): Tree[_ <: Polarity] =
    Option(expr.tokVar).map(tokVar => VarTree(table.getOrElseUpdate(tokVar.getText, fresh))) orElse
    Option(expr.atomic).map(fromAtomic) orElse
    Option(expr.expr).map(fromExpr) orElse
    Option(expr.funExpr).map(fromFunExpr) orElse
    Option(expr.ifExpr).map(fromIfExpr) orElse
    Option(expr.tuple).map(makeTuple) orElse
    Option(expr.list).map(makeList) getOrElse (throw new RuntimeException)

  def fromExpr100(expr: ErlangParser.Expr100Context): Tree[_ <: Polarity] = {
    val exprs = expr.expr150.asScala.map(fromExpr150)
    val ops = expr.matchOrSend.asScala.map(_.getText)
    makeMatchOrSend(exprs, ops)
  }

  def fromExpr150(expr: ErlangParser.Expr150Context): Tree[_ <: Polarity] = {
    val exprs = expr.expr160.asScala.map(fromExpr160)
    makeBinOpCall(exprs, Seq.fill(exprs.size - 1)("orelse"))
  }

  def fromExpr160(expr: ErlangParser.Expr160Context): Tree[_ <: Polarity] = {
    val exprs = expr.expr200.asScala.map(fromExpr200)
    makeBinOpCall(exprs, Seq.fill(exprs.size - 1)("andalso"))
  }

  def fromExpr200(expr: ErlangParser.Expr200Context): Tree[_ <: Polarity] = makeBinOpCall(expr.expr300.asScala.map(fromExpr300), Option(expr.compOp).map(_.getText).toList)

  def fromExpr300(expr: ErlangParser.Expr300Context): Tree[_ <: Polarity] = makeBinOpCall(expr.expr400.asScala.map(fromExpr400), expr.listOp.asScala.map(_.getText))

  def fromExpr400(expr: ErlangParser.Expr400Context): Tree[_ <: Polarity] = makeBinOpCall(expr.expr500.asScala.map(fromExpr500), expr.addOp.asScala.map(_.getText))

  def fromExpr500(expr: ErlangParser.Expr500Context): Tree[_ <: Polarity] = makeBinOpCall(expr.expr600.asScala.map(fromExpr600), expr.multOp.asScala.map(_.getText))

  def fromExpr600(expr: ErlangParser.Expr600Context): Tree[_ <: Polarity] =
    Option(expr.prefixOp).map(prefixOp => FunCallTree(FunRefTree(prefixOp.getText, 1), List(untype(fromExpr700(expr.expr700))))) getOrElse
    fromExpr700(expr.expr700)

  def fromExpr700(expr: ErlangParser.Expr700Context): Tree[_ <: Polarity] =
    Option(expr.expr800).map(fromExpr800) orElse
    Option(expr.functionCall).map(fromFunctionCall) getOrElse (throw new RuntimeException)

  def fromExpr800(expr: ErlangParser.Expr800Context): Tree[_ <: Polarity] = fromExprMax(expr.exprMax.get(0))

  def fromExpr(expr: ErlangParser.ExprContext): Tree[_ <: Polarity] =
    fromExpr100(expr.expr100)

  def fromFunctionCall(funCall: ErlangParser.FunctionCallContext): Tree[_ <: Polarity] = {
    val args = Option(funCall.argumentList.exprs).toList.flatMap(_.expr.asScala.map(expr => untype[Pos](fromExpr(expr))))
    val f = fromExpr800(funCall.expr800) match {
      case AtomTree(name) => FunRefTree(name, args.size)
      case expr => expr
    }
    FunCallTree(untype(f), args)
  }

  def fromFunExpr(fun: ErlangParser.FunExprContext): Tree[_ <: Polarity] =
    Option(fun.funClauses).map(funClauses => FunTree(funClauses.funClause.asScala.map(clause => makeFunClause(None, clause.argumentList, clause.clauseGuard, clause.clauseBody))(collection.breakOut))) orElse
    Option(fun.tokAtom).map(tokAtom => FunRefTree(tokAtom.getText, fun.tokInteger.getText.toInt)) getOrElse (throw new RuntimeException)

  def makeIfClause(guard: Option[ErlangParser.GuardContext], body: ErlangParser.ClauseBodyContext): IfClauseTree =
    IfClauseTree(
      guard.toList.flatMap(_.exprs.asScala.flatMap(_.expr.asScala.map(expr => untype[Pos](fromExpr(expr)))(collection.breakOut))),
      body.exprs.expr.asScala.map(expr => untype[Pos](fromExpr(expr)))(collection.breakOut)
    )

  def fromIfExpr(expr: ErlangParser.IfExprContext): Tree[_ <: Polarity] =
    IfTree(
      expr.ifClauses.ifClause.asScala.map(clause => makeIfClause(Some(clause.guard), clause.clauseBody))(collection.breakOut)
    )

  def makeTuple[A <: Polarity](tuple: ErlangParser.TupleContext): TupleTree[A] =
    TupleTree(Option(tuple.exprs).toList.flatMap(_.expr.asScala.map(expr => untype[A](fromExpr(expr)))))

  def makeList[A <: Polarity](list: ErlangParser.ListContext): ListTree[A] =
    Option(list.expr).map { expr =>
      makeTail(untype[A](fromExpr(expr)), list.tail)
    }.getOrElse {
      ListTree(Nil, None)
    }

  def makeTail[A <: Polarity](head: Tree[A], tail: ErlangParser.TailContext): ListTree[A] =
    Option(tail.expr).map { expr =>
      Option(tail.tail).map { tail =>
        val ListTree(v, t) = makeTail(untype[A](fromExpr(expr)), tail)
        ListTree(head :: v, t)
      }.getOrElse {
        ListTree(List(head), Some(untype[A](fromExpr(expr))))
      }
    }.getOrElse {
      ListTree(List(head), None)
    }

  def makeFunClause(name: Option[String], args: ErlangParser.ArgumentListContext, guards: ErlangParser.ClauseGuardContext, body: ErlangParser.ClauseBodyContext): FunClauseTree =
    FunClauseTree(
      name,
      Option(args.exprs).toList.flatMap(_.expr.asScala.map(expr => untype[Neg](fromExpr(expr)))),
      makeIfClause(Option(guards.guard), body)
    )

  def makeBinOpCall(exprs: Seq[Tree[_ <: Polarity]], ops: Seq[String]): Tree[_ <: Polarity] = {
    val head +: tail = exprs
    tail.zip(ops).foldLeft(head) {
      case (lhs, (rhs, op)) => untype(FunCallTree(FunRefTree(op, 2), List(untype(lhs), untype(rhs))))
    }
  }

  def makeMatchOrSend(exprs: Seq[Tree[_ <: Polarity]], ops: Seq[String]): Tree[_ <: Polarity] = {
    val init :+ last = exprs
    init.zip(ops).foldRight(last) {
      case ((lhs, op), rhs) =>
        if (op == "=")
          MatchTree(untype(lhs), rhs)
        else
          untype(FunCallTree(FunRefTree(op, 2), List(untype(lhs), untype(rhs))))
    }
  }

  def getResult: Vector[Tree[_ <: Polarity]] = result.toVector

}
