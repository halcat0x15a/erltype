package erltype

case class TypingScheme[A](env: Delta, decls: Vars, typ: A) {

  def get: TypingScheme[(Delta, A)] = TypingScheme(env, decls, (env, typ))

  def map[B](f: A => B): TypingScheme[B] = TypingScheme(env, decls, f(typ))

  def flatMap[B](f: A => TypingScheme[B]): TypingScheme[B] = {
    val scheme = f(typ)
    val delta = env.foldLeft(scheme.env) {
      case (env, (name, typ)) =>
        env.updated(name, env.get(name).fold(typ)(typ /\ _))
    }
    TypingScheme(delta, decls ++ scheme.decls, scheme.typ)
  }

  def withFilter(p: A => Boolean): TypingScheme[A] = this

}

object TypingScheme {

  def apply[A](typ: A): TypingScheme[A] = TypingScheme(Map.empty, Vector.empty, typ)

  def apply[A](env: Delta, typ: A): TypingScheme[A] = TypingScheme(env, Vector.empty, typ)

  def inst[A <: Polarity](scheme: TypingScheme[(Type[A], Type[Pos], Type[Neg])])(implicit A: A): TypingScheme[Type[A]] = {
    val TypingScheme(env, decls, (typ, pos, neg)) = scheme
    val subst = Type.biunify(pos, neg)
    TypingScheme(env.mapValues(subst(_)), decls, subst(typ))
  }

  def simplify[A <: Polarity](scheme: TypingScheme[Type[A]]): TypingScheme[Type[A]] = {
    val TypingScheme(env, decls, typ) = scheme
    TypingScheme(env -- decls, Vector.empty, typ)
  }

  implicit class TypeOp[A <: Polarity](val self: TypingScheme[Type[A]]) {
    def pretty(implicit A: A): TypingScheme[Type[A]] = {
      val scheme = simplify(self)
      TypingScheme(scheme.env.mapValues(Type.pretty(_)), Type.pretty(scheme.typ))
    }
    def show: String = {
      val delta = self.env.map { case (k, v) => s"${VarType(k).show}: ${v.show}" }.mkString(", ")
      s"[$delta]${self.typ.show}"
    }
  }

}
