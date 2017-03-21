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
  def flatMapWithEnv[B](f: (Delta, A) => TypingScheme[B]): TypingScheme[B] = flatMap(a => f(env, a))
}

object TypingScheme {

  def show[A <: Polarity](scheme: TypingScheme[ErlType[A]]): String = {
    val delta = scheme.env.map { case (k, v) => s"$k: ${v.show}" }.mkString(", ")
    s"[$delta]${scheme.typ.show}"
  }

  implicit class PlusOp(val scheme: TypingScheme[ErlType[Plus]]) {
    def inst(x: ErlType[Plus], y: ErlType[Minus]): TypingScheme[ErlType[Plus]] = {
      val subst = ErlType.biunify(x, y)
      TypingScheme(scheme.env.mapValues(subst(_)), subst(scheme.typ))
    }
    def simplify: TypingScheme[ErlType[Plus]] = TypingScheme(scheme.env.mapValues(ErlType.simplify(_)), ErlType.simplify(scheme.typ))
  }

  implicit class MinusOp(val scheme: TypingScheme[ErlType[Minus]]) {
    def inst(x: ErlType[Plus], y: ErlType[Minus]): TypingScheme[ErlType[Minus]] = {
      val subst = ErlType.biunify(x, y)
      TypingScheme(scheme.env.mapValues(subst(_)), subst(scheme.typ))
    }
  }
}
