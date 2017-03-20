package erltype

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

class Analyzer extends ErlangBaseListener {

  private var result: Buffer[ErlTree] = Buffer.empty

  override def exitFunction(ctx: ErlangParser.FunctionContext): Unit = {
    val clauses = ctx.functionClause.asScala
    result += FunTree(Some(clauses(0).tokAtom.getText), clauses.map(clause => makeFunClause(clause.clauseArgs.argumentList, clause.clauseBody))(collection.breakOut))
  }

  def fromAtomic(atomic: ErlangParser.AtomicContext): ErlTree =
    Option(atomic.tokChar).map(tokChar => CharTree(tokChar.getText.charAt(0))) orElse
    Option(atomic.tokInteger).map(tokInteger => IntTree(tokInteger.getText.toInt)) orElse
    Option(atomic.tokFloat).map(tokFloat => FloatTree(tokFloat.getText.toFloat)) orElse
    Option(atomic.tokAtom).map(tokAtom => AtomTree(tokAtom.getText)) orElse
    Option(atomic.tokString).map(tokString => StringTree(tokString.asScala.map(_.getText).mkString)) getOrElse (throw new RuntimeException)

  def fromExprMax(expr: ErlangParser.ExprMaxContext): ErlTree =
    Option(expr.tokVar).map(tokVar => VarTree(tokVar.getText)) orElse
    Option(expr.atomic).map(fromAtomic) orElse
    Option(expr.expr).map(fromExpr) orElse
    Option(expr.funExpr).map(fromFunExpr) orElse
    Option(expr.list).map(list => ListTree(makeList(list)(_.expr)(_.tail))) getOrElse (throw new RuntimeException)

  def fromExpr700(expr: ErlangParser.Expr700Context): ErlTree =
    Option(expr.expr800).map(fromExpr800) orElse
    Option(expr.functionCall).map(fromFunctionCall) getOrElse (throw new RuntimeException)

  def fromExpr800(expr: ErlangParser.Expr800Context): ErlTree = fromExprMax(expr.exprMax.get(0))

  def fromExpr(expr: ErlangParser.ExprContext): ErlTree =
    fromExpr700(expr.expr100.
      expr150.get(0).
      expr160.get(0).
      expr200.get(0).
      expr300.get(0).
      expr400.get(0).
      expr500.get(0).
      expr600.get(0).
      expr700)

  def fromFunctionCall(funCall: ErlangParser.FunctionCallContext): ErlTree =
    FunCallTree(fromExpr800(funCall.expr800), Option(funCall.argumentList.exprs).toList.flatMap(_.expr.asScala.map(fromExpr)))

  def fromFunExpr(fun: ErlangParser.FunExprContext): ErlTree =
    Option(fun.funClauses).map(funClauses => FunTree(None, funClauses.funClause.asScala.map(clause => makeFunClause(clause.argumentList, clause.clauseBody))(collection.breakOut))) orElse
    Option(fun.tokAtom).map(tokAtom => FunRefTree(tokAtom.getText, fun.tokInteger.getText.toInt)) getOrElse (throw new RuntimeException)

  def makeList[A](a: A)(head: A => ErlangParser.ExprContext)(tail: A => ErlangParser.TailContext): List[ErlTree] =
    Option(head(a)).map(head => fromExpr(head) :: makeList(tail(a))(_.expr)(_.tail)).getOrElse(Nil)

  def makeFunClause(args: ErlangParser.ArgumentListContext, body: ErlangParser.ClauseBodyContext): FunClauseTree =
    FunClauseTree(
      Option(args.exprs).toList.flatMap(_.expr.asScala.map(fromExpr)),
      Nil, // TODO
      body.exprs.expr.asScala.map(fromExpr)(collection.breakOut)
    )

  def getResult: Vector[ErlTree] = result.toVector

}
