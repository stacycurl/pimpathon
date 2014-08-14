package pimpathon

object function {
  type Predicate[-A] = A => Boolean

  implicit def predicateOps[A](p: Predicate[A]): PredicateOps[A] = new PredicateOps[A](p)

  class PredicateOps[A](p: Predicate[A]) {
    def and(q: Predicate[A]): Predicate[A] = (a: A) => (p(a) && q(a))
    def or(q: Predicate[A]):  Predicate[A] = (a: A) => (p(a) || q(a))
    def not:                  Predicate[A] = (a: A) => (!p(a))

    def exists: Predicate[List[A]] = (_.exists(p))
    def forall: Predicate[List[A]] = (_.forall(p))

    def ifSome: Predicate[Option[A]] = (_.exists(p))
  }

  def equalC[A]: A => A => Boolean = (l: A) => (r: A) => l equals r
}
