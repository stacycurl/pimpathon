package pimpathon

import scala.language.higherKinds

import scala.collection.{GenTraversable, GenTraversableLike}

import pimpathon.list._


object function {
  type Predicate[-A] = A => Boolean

  implicit class FunctionOps[A, B](val f: A => B) extends AnyVal {
    def guardWith(p: Predicate[A]): PartialFunction[A, B] = p guard f
  }

  implicit class CurriedFunction2Ops[A, B, C](val f: A => B => C) extends AnyVal {
    def tupled: ((A, B)) => C = Function.uncurried(f).tupled
  }

  implicit class PredicateOps[A](val p: Predicate[A]) extends AnyVal {
    def and(q: Predicate[A]): Predicate[A] = (a: A) => p(a) && q(a)
    def or(q: Predicate[A]):  Predicate[A] = (a: A) => p(a) || q(a)
    def not:                  Predicate[A] = (a: A) => !p(a)

    def exists: Predicate[List[A]] = _.exists(p)
    def forall: Predicate[List[A]] = _.forall(p)

    def ifSome: Predicate[Option[A]] = _.exists(p)

    def guard[B](f: A => B): PartialFunction[A, B] = new GuardedPartialFunction[A, B](p, f)
  }

  implicit class PartialFunctionOps[In, Out](pf: PartialFunction[In, Out]) {
    def partition[CC[A] <: GenTraversableLike[A, GenTraversable[A]]](ins: CC[In])
      (implicit cbf: CCBF[Either[In, Out], CC], icbf: CCBF[In, CC], ocbf: CCBF[Out, CC]): (CC[In], CC[Out]) =
        ins.map(either).partitionEithers[CC](icbf, ocbf)

    def either: In => Either[In, Out] = toRight
    def toRight: In => Either[In, Out] = (in: In) => pf.lift(in).toRight(in)
    def toLeft:  In => Either[Out, In] = (in: In) => pf.lift(in).toLeft(in)

    def first[C]: PartialFunction[(In, C), (Out, C)] = ***(identityPF[C])

    def ***[In2, Out2](rhs: PartialFunction[In2, Out2]): PartialFunction[(In, In2), (Out, Out2)] =
      new PartialFunction[(In, In2), (Out, Out2)] {
        def isDefinedAt(in: (In, In2)): Boolean = pf.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
        def apply(in: (In, In2)): (Out, Out2) = (pf.apply(in._1), rhs.apply(in._2))
      }
  }

  implicit class PartialEndoFunctionOps[A](val pf: PartialFunction[A, A]) extends AnyVal {
    def unify: A => A = (a: A) => pf.lift(a).getOrElse(a)
  }

  private class GuardedPartialFunction[A, B](p: Predicate[A], f: A => B) extends PartialFunction[A, B] {
    def isDefinedAt(a: A): Boolean = p(a)
    def apply(a: A): B = f(a)
  }

  def identityPF[A]: PartialFunction[A, A] = PartialFunction(identity[A])
  def equalC[A]: A => A => Boolean = (l: A) => (r: A) => l equals r
}
