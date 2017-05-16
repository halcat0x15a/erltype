package erltype

case class TypingScheme[A](env: Delta, typ: A) {

  def map[B](f: A => B): TypingScheme[B] = TypingScheme(env, f(typ))

  def flatMap[B](f: A => TypingScheme[B]): TypingScheme[B] = {
    val scheme = f(typ)
    val delta = env.foldLeft(scheme.env) {
      case (env, (name, typ)) =>
        env.updated(name, env.get(name).fold(typ)(typ /\ _))
    }
    TypingScheme(delta, scheme.typ)
  }

  def zip[B](state: SchemeState[B]): TypingScheme[(A, B)] =
    state(env).map((typ, _))

}

object TypingScheme {

  def get[A <: Polarity](env: Delta, typ: Type[Pos]): Type[Neg] = typ match {
    case VarType(id) => env.getOrElse(id, TopType)
    case ListType(typ) => ListType(get(env, typ))
    case TupleType(types) => TupleType(types.map(get(env, _)))
    case FunctionType(params, ret) => FunctionType(params.map(typ => get(env, typ.inverse).inverse), get(env, ret))
    case UnionType(types) => IntersectionType(types.map(get(env, _)))
    case typ => typ.inverse
  }

  implicit class TypeOp[A <: Polarity](val scheme: TypingScheme[Type[A]]) {
    def inst(x: Type[Pos], y: Type[Neg])(implicit A: A): TypingScheme[Type[A]] = {
      val subst = Type.biunify(x, y)
      TypingScheme(scheme.env.mapValues(subst(_)), subst(scheme.typ))
    }
    def simplify(implicit A: A): TypingScheme[Type[A]] = {
      TypingScheme(scheme.env.mapValues(Type.simplify(_)), Type.simplify(scheme.typ))
    }
    def show: String = {
      val delta = scheme.env.map { case (k, v) => s"${VarType(k).show}: ${v.show}" }.mkString(", ")
      s"[$delta]${scheme.typ.show}"
    }
  }

}
