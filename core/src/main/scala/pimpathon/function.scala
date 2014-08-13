package pimpathon

object function {
  type Predicate[-A] = A => Boolean

  implicit class PredicateOps[A](val p: Predicate[A]) extends AnyVal {
    def and(q: Predicate[A]): Predicate[A] = (a: A) => (p(a) && q(a))
    def or(q: Predicate[A]):  Predicate[A] = (a: A) => (p(a) || q(a))
    def not:                  Predicate[A] = (a: A) => (!p(a))

    def exists: Predicate[List[A]] = (_.exists(p))
    def forall: Predicate[List[A]] = (_.forall(p))

    def ifSome: Predicate[Option[A]] = (_.exists(p))
  }
}
