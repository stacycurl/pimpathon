package pimpathon

object function {
  implicit class PredicateOps[A](val p: A => Boolean) extends AnyVal {
    def and(q: A => Boolean): (A => Boolean) = (a: A) => (p(a) && q(a))
    def or(q: A => Boolean):  (A => Boolean) = (a: A) => (p(a) || q(a))

    def exists: List[A] => Boolean = (_.exists(p))
    def forall: List[A] => Boolean = (_.forall(p))
  }
}
