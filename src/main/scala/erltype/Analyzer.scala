package erltype

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

class Analyzer extends ErlangBaseListener {

  private var result: Buffer[ErlTree] = Buffer.empty

  override def exitFunction(ctx: ErlangParser.FunctionContext): Unit = {
    val clauses = ctx.functionClause.asScala
    val tree = FunTree(Some(clauses(0).tokAtom.getText), clauses.map(clause => makeFunClause(clause.clauseArgs.argumentList, clause.clauseBody))(collection.breakOut))
    result += tree
  }

  def fromAtomic(atomic: ErlangParser.AtomicContext): ErlTree =
    Option(atomic.tokChar).map(tokChar => CharTree(tokChar.getText.charAt(0))) orElse
    Option(atomic.tokInteger).map(tokInteger => IntTree(tokInteger.getText.toInt)) orElse
    Option(atomic.tokFloat).map(tokFloat => FloatTree(tokFloat.getText.toFloat)) orElse
    Option(atomic.tokAtom).map(tokAtom => AtomTree(tokAtom.getText)) orElse
    Option(atomic.tokString).map(tokString => StringTree(tokString.asScala.map(_.getText).mkString)) getOrElse (throw new RuntimeException)

  def fromExprMax(expr: ErlangParser.ExprMaxContext): ErlTree =
    Option(expr.tokVar).map(tokVar => VarTree(tokVar.getText.hashCode)) orElse
    Option(expr.atomic).map(fromAtomic) orElse
    Option(expr.expr).map(fromExpr) orElse
    Option(expr.funExpr).map(fromFunExpr) orElse
    Option(expr.list).map(makeList) getOrElse (throw new RuntimeException)

  def fromExpr100(expr: ErlangParser.Expr100Context): ErlTree = {
    val head +: tail = expr.expr150.asScala.map(fromExpr150)
    tail.foldRight(head)(AssignTree(_, _))
  }

  def fromExpr150(expr: ErlangParser.Expr150Context): ErlTree = {
    val exprs = expr.expr160.asScala.map(fromExpr160)
    makeBinOpCall(exprs, Seq.fill(exprs.size - 1)("orelse"))
  }

  def fromExpr160(expr: ErlangParser.Expr160Context): ErlTree = {
    val exprs = expr.expr200.asScala.map(fromExpr200)
    makeBinOpCall(exprs, Seq.fill(exprs.size - 1)("andalso"))
  }

  def fromExpr200(expr: ErlangParser.Expr200Context): ErlTree = makeBinOpCall(expr.expr300.asScala.map(fromExpr300), Option(expr.compOp).map(_.getText).toList)

  def fromExpr300(expr: ErlangParser.Expr300Context): ErlTree = makeBinOpCall(expr.expr400.asScala.map(fromExpr400), expr.listOp.asScala.map(_.getText))

  def fromExpr400(expr: ErlangParser.Expr400Context): ErlTree = makeBinOpCall(expr.expr500.asScala.map(fromExpr500), expr.addOp.asScala.map(_.getText))

  def fromExpr500(expr: ErlangParser.Expr500Context): ErlTree = makeBinOpCall(expr.expr600.asScala.map(fromExpr600), expr.multOp.asScala.map(_.getText))

  def fromExpr600(expr: ErlangParser.Expr600Context): ErlTree =
    Option(expr.prefixOp).map(prefixOp => FunCallTree(AtomTree(prefixOp.getText), List(fromExpr700(expr.expr700)))) getOrElse
    fromExpr700(expr.expr700)

  def fromExpr700(expr: ErlangParser.Expr700Context): ErlTree =
    Option(expr.expr800).map(fromExpr800) orElse
    Option(expr.functionCall).map(fromFunctionCall) getOrElse (throw new RuntimeException)

  def fromExpr800(expr: ErlangParser.Expr800Context): ErlTree = fromExprMax(expr.exprMax.get(0))

  def fromExpr(expr: ErlangParser.ExprContext): ErlTree =
    fromExpr100(expr.expr100)

  def fromFunctionCall(funCall: ErlangParser.FunctionCallContext): ErlTree =
    FunCallTree(fromExpr800(funCall.expr800), Option(funCall.argumentList.exprs).toList.flatMap(_.expr.asScala.map(fromExpr)))

  def fromFunExpr(fun: ErlangParser.FunExprContext): ErlTree =
    Option(fun.funClauses).map(funClauses => FunTree(None, funClauses.funClause.asScala.map(clause => makeFunClause(clause.argumentList, clause.clauseBody))(collection.breakOut))) orElse
    Option(fun.tokAtom).map(tokAtom => FunRefTree(tokAtom.getText, fun.tokInteger.getText.toInt)) getOrElse (throw new RuntimeException)

  def makeList[A](list: ErlangParser.ListContext): ErlTree =
    Option(list.expr).map { expr =>
      makeTail(fromExpr(expr), list.tail)
    }.getOrElse {
      NilTree
    }

  def makeTail[A](head: ErlTree, tail: ErlangParser.TailContext): ErlTree =
    Option(tail.expr).map { expr =>
      Option(tail.tail).map { tail =>
        ConsTree(head, makeTail(fromExpr(expr), tail))
      }.getOrElse {
        ConsTree(head, fromExpr(expr))
      }
    }.getOrElse {
      ConsTree(head, NilTree)
    }

  def makeFunClause(args: ErlangParser.ArgumentListContext, body: ErlangParser.ClauseBodyContext): FunClauseTree =
    FunClauseTree(
      Option(args.exprs).toList.flatMap(_.expr.asScala.map(fromExpr)),
      Nil, // TODO
      body.exprs.expr.asScala.map(fromExpr)(collection.breakOut)
    )

  def makeBinOpCall(exprs: Seq[ErlTree], ops: Seq[String]): ErlTree = {
    val head +: tail = exprs
    tail.zip(ops).foldLeft(head) {
      case (lhs, (rhs, op)) => FunCallTree(AtomTree(op), List(lhs, rhs))
    }
  }

  def getResult: Vector[ErlTree] = result.toVector

}
