package pimpathon


object function {
  type Predicate[-A] = A => Boolean

  implicit def predicateOps[A](p: Predicate[A]): PredicateOps[A] = new PredicateOps[A](p)

  class PredicateOps[A](p: Predicate[A]) {
    def and(q: Predicate[A]): Predicate[A] = (a: A) => p(a) && q(a)
    def or(q: Predicate[A]):  Predicate[A] = (a: A) => p(a) || q(a)
    def not:                  Predicate[A] = (a: A) => !p(a)

    def exists: Predicate[List[A]] = _.exists(p)
    def forall: Predicate[List[A]] = _.forall(p)

    def ifSome: Predicate[Option[A]] = _.exists(p)
  }

  implicit def partialFunctionOps[In, Out](pf: PartialFunction[In, Out]): PartialFunctionOps[In, Out] =
    new PartialFunctionOps[In, Out](pf)

  class PartialFunctionOps[In, Out](pf: PartialFunction[In, Out]) {
    def either: In => Either[In, Out] = toRight
    def toRight: In => Either[In, Out] = (in: In) => pf.lift(in).toRight(in)
    def toLeft:  In => Either[Out, In] = (in: In) => pf.lift(in).toLeft(in)

    def ***[In2, Out2](rhs: PartialFunction[In2, Out2]): PartialFunction[(In, In2), (Out, Out2)] =
      new PartialFunction[(In, In2), (Out, Out2)] {
        def isDefinedAt(in: (In, In2)): Boolean = pf.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
        def apply(in: (In, In2)): (Out, Out2) = (pf.apply(in._1), rhs.apply(in._2))
      }
  }

  def equalC[A]: A => A => Boolean = (l: A) => (r: A) => l equals r
}
