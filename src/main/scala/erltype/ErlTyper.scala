package erltype

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
  }

}

trait ErlTyper[A] {
  def check_+(env: Pi, ctx: A): TypingScheme[ErlType[Plus]]
  def check_-(env: Delta, ctx: A): TypingScheme[ErlType[Minus]]
}

object ErlTyper extends ErlTypers
