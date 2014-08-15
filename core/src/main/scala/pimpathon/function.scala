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

  implicit class PartialFunctionOps[-In, +Out](pf: PartialFunction[In, Out]) {
    def ***[In2, Out2](rhs: PartialFunction[In2, Out2]): PartialFunction[(In, In2), (Out, Out2)] =
      new PartialFunction[(In, In2), (Out, Out2)] {
        def isDefinedAt(in: (In, In2)): Boolean = pf.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
        def apply(in: (In, In2)): (Out, Out2) = (pf.apply(in._1), rhs.apply(in._2))
      }
  }

  def equalC[A]: A => A => Boolean = (l: A) => (r: A) => l equals r
}
