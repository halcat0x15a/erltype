package erltype

trait SchemeState[A] { state =>

  def apply(env: Delta): TypingScheme[A]

  def map[B](f: A => B): SchemeState[B] = flatMap(a => SchemeState(f(a)))

  def flatMap[B](f: A => SchemeState[B]): SchemeState[B] =
    new SchemeState[B] {
      def apply(env: Delta): TypingScheme[B] = {
        val TypingScheme(delta, a) = state(env)
        f(a)(delta)
      }
    }

}

object SchemeState {

  def apply[A](a: A): SchemeState[A] =
    new SchemeState[A] {
      def apply(env: Delta): TypingScheme[A] = TypingScheme(env, a)
    }

}
