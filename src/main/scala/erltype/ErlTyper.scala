package erltype

case class TypingScheme[A](env: Map[String, ErlType[Minus]], typ: A) {
  def map[B](f: A => B): TypingScheme[B] = flatMap(a => TypingScheme(Map.empty, f(a)))
  def flatMap[B](f: A => TypingScheme[B]): TypingScheme[B] = {
    val scheme = f(typ)
    val delta = env.foldLeft(scheme.env) {
      case (env, (name, typ)) => env.updated(name, env.get(name).fold(typ)(typ /\ _))
    }
    TypingScheme(delta, scheme.typ)
  }
}

trait ErlTyper[A] {
  def check(env: Map[String, TypingScheme[ErlType[Plus]]], ctx: A): TypingScheme[ErlType[Plus]]
}

object ErlTyper extends ErlTypers {

  def inst(scheme: TypingScheme[ErlType[Plus]], x: ErlType[Plus], y: ErlType[Minus]): TypingScheme[ErlType[Plus]] = {
    val subst = ErlType.biunify(x, y)
    TypingScheme(scheme.env.mapValues(subst(_)), subst(scheme.typ))
  }

  def show(scheme: TypingScheme[ErlType[Plus]]): String =
    s"""[${scheme.env.map { case (k, v) => s"$k : ${v.show}" }.mkString(", ")}](${scheme.typ.show})"""

}
