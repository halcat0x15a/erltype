package erltype

package typer

trait TokChar extends ErlTyper[ErlangParser.TokCharContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokCharContext) = Success(env, ErlChar)
}

trait TokInteger extends ErlTyper[ErlangParser.TokIntegerContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokIntegerContext) = Success(env, ErlInteger)
}

trait TokFloat extends ErlTyper[ErlangParser.TokFloatContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokFloatContext) = Success(env, ErlFloat)
}

trait TokAtom extends ErlTyper[ErlangParser.TokAtomContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokAtomContext) = Success(env, ErlAtom(ctx.getText))
}

trait TokString extends ErlTyper[ErlangParser.TokStringContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokStringContext) = Success(env, ErlString)
}

trait TokVar extends ErlTyper[ErlangParser.TokVarContext] {
  def check(env: TypeEnv, ctx: ErlangParser.TokVarContext) = {
    val name = ctx.getText
    env.get(name).map(Success(env, _)).getOrElse {
      val erlVar = ErlVar(name)
      Success(env.updated(name, erlVar), erlVar)
    }
  }
}

trait Atomic extends ErlTyper[ErlangParser.AtomicContext] {
  def check(env: TypeEnv, ctx: ErlangParser.AtomicContext) =
    ctx.tokChar.check(env) orElse
    ctx.tokInteger.check(env) orElse
    ctx.tokFloat.check(env) orElse
    ctx.tokAtom.check(env) orElse
    Option(ctx.tokString).toResult(env).map(_ => ErlString)
}
