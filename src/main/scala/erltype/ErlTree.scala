package erltype

sealed abstract class ErlTree

case class IntTree(value: Int) extends ErlTree

case class FloatTree(value: Float) extends ErlTree

case class CharTree(value: Char) extends ErlTree

case class AtomTree(value: String) extends ErlTree

case class StringTree(value: String) extends ErlTree

case class VarTree(value: String) extends ErlTree

case class ListTree(value: List[ErlTree]) extends ErlTree

case class FunClauseTree(args: List[ErlTree], guards: List[List[ErlTree]], body: List[ErlTree]) extends ErlTree

case class FunTree(name: Option[String], clauses: List[FunClauseTree]) extends ErlTree

case class FunRefTree(name: String, arity: Int) extends ErlTree

case class FunCallTree(fun: ErlTree, args: List[ErlTree]) extends ErlTree
