package pimpathon

import scala.collection.{GenTraversable, GenTraversableLike}

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.list._


object function {
  type Predicate[-A] = A ⇒ Boolean

  implicit def functionPimps[A, B](f: A ⇒ B): FunctionPimps[A, B] = new FunctionPimps[A, B](f)

  implicit def curriedFunction2Pimps[A, B, C](f: A ⇒ B ⇒ C): CurriedFunction2Pimps[A, B, C] =
    new CurriedFunction2Pimps[A, B, C](f)

  implicit def predicatePimps[A](p: Predicate[A]): PredicatePimps[A] = new PredicatePimps[A](p)

  implicit def partialFunctionPimps[In, Out](pf: PartialFunction[In, Out]): PartialFunctionPimps[In, Out] =
    new PartialFunctionPimps[In, Out](pf)

  implicit def partialEndoFunctionPimps[A](pf: PartialFunction[A, A]): PartialEndoFunctionPimps[A] =
    new PartialEndoFunctionPimps[A](pf)

  class FunctionPimps[A, B](f: A ⇒ B) {
    def attempt: A ⇒ Either[Throwable, B] = _.attempt(f)
    def guardWith(p: Predicate[A]): PartialFunction[A, B] = p guard f
  }

  class CurriedFunction2Pimps[A, B, C](f: A ⇒ B ⇒ C) {
    def tupled: ((A, B)) ⇒ C = Function.uncurried(f).tupled
  }

  class PredicatePimps[A](p: Predicate[A]) {
    def cond[B](ifTrue: ⇒ B, ifFalse: ⇒ B): A ⇒ B = a ⇒ p(a).cond(ifTrue, ifFalse)
    def and(q: Predicate[A]): Predicate[A] = (a: A) ⇒ p(a) && q(a)
    def or(q: Predicate[A]):  Predicate[A] = (a: A) ⇒ p(a) || q(a)
    def not:                  Predicate[A] = (a: A) ⇒ !p(a)

    def exists: Predicate[List[A]] = _.exists(p)
    def forall: Predicate[List[A]] = _.forall(p)

    def ifSome: Predicate[Option[A]] = _.exists(p)

    def guard[B](f: A ⇒ B): PartialFunction[A, B] = new GuardedPartialFunction[A, B](p, f)
  }

  class PartialFunctionPimps[In, Out](pf: PartialFunction[In, Out]) {
    def isUndefinedAt(in: In): Boolean = !pf.isDefinedAt(in)

    def partition[CC[A]](ins: GenTraversableLike[In, GenTraversable[In]])
      (implicit cbf: CCBF[Either[In, Out], CC], icbf: CCBF[In, CC], ocbf: CCBF[Out, CC]): (CC[In], CC[Out]) =
        ins.map(either).partitionEithers[CC](icbf, ocbf)

    def either:  In ⇒ Either[In, Out] = toRight
    def toRight: In ⇒ Either[In, Out] = (in: In) ⇒ pf.lift(in).toRight(in)
    def toLeft:  In ⇒ Either[Out, In] = (in: In) ⇒ pf.lift(in).toLeft(in)

    def first[C]:  PartialFunction[(In, C), (Out, C)] = ***(identityPF[C])
    def second[C]: PartialFunction[(C, In), (C, Out)] = identityPF[C] *** pf

    def ***[In2, Out2](rhs: PartialFunction[In2, Out2]): PartialFunction[(In, In2), (Out, Out2)] =
      new PartialFunction[(In, In2), (Out, Out2)] {
        def isDefinedAt(in: (In, In2)): Boolean = pf.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
        def apply(in: (In, In2)): (Out, Out2) = (pf.apply(in._1), rhs.apply(in._2))
      }
  }

  class PartialEndoFunctionPimps[A](pf: PartialFunction[A, A]) {
    def unify: A ⇒ A = (a: A) ⇒ pf.lift(a).getOrElse(a)
  }

  private class GuardedPartialFunction[A, B](p: Predicate[A], f: A ⇒ B) extends PartialFunction[A, B] {
    def isDefinedAt(a: A): Boolean = p(a)
    def apply(a: A): B = f(a)
  }

  def identityPF[A]: PartialFunction[A, A] = { case a ⇒ a }
  def equalC[A]: A ⇒ A ⇒ Boolean = (l: A) ⇒ (r: A) ⇒ l equals r
  def nand[A](ps: Predicate[A]*): Predicate[A] = and(ps: _*).not
  def nor[A](ps: Predicate[A]*): Predicate[A]  = or(ps: _*).not
  def or[A](ps: Predicate[A]*): Predicate[A]   = ps.foldLeft((a: A) ⇒ false)(_ or _)
  def and[A](ps: Predicate[A]*): Predicate[A]  = ps.foldLeft((a: A) ⇒ true)(_ and _)
}