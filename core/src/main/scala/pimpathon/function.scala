package pimpathon

import scala.language.higherKinds

import scala.{PartialFunction ⇒ ~>}
import scala.collection.{GenTraversable, GenTraversableLike}

import pimpathon.boolean._
import pimpathon.list._

import scala.util.Try


object function {
  type Predicate[-A] = A ⇒ Boolean

  implicit class FunctionPimps[A, B](val f: A ⇒ B) extends AnyVal {
    def attempt: A ⇒ Try[B] = a ⇒ Try(f(a))
    def guardWith(p: Predicate[A]): A ~> B = p guard f
  }

  implicit class CurriedFunction2Pimps[A, B, C](val f: A ⇒ B ⇒ C) extends AnyVal {
    def tupled: ((A, B)) ⇒ C = Function.uncurried(f).tupled
  }

  implicit class PredicatePimps[A](val p: Predicate[A]) extends AnyVal {
    def cond[B](ifTrue: ⇒ B, ifFalse: ⇒ B): A ⇒ B = a ⇒ p(a).cond(ifTrue, ifFalse)

    def and(q: Predicate[A]): Predicate[A] = (a: A) ⇒ p(a) && q(a)
    def or(q: Predicate[A]):  Predicate[A] = (a: A) ⇒ p(a) || q(a)
    def not:                  Predicate[A] = (a: A) ⇒ !p(a)

    def exists: Predicate[List[A]] = _.exists(p)
    def forall: Predicate[List[A]] = _.forall(p)

    def ifSome: Predicate[Option[A]] = _.exists(p)

    def guard[B](f: A ⇒ B): A ~> B = new GuardedPartialFunction[A, B](p, f)
  }

  implicit class PartialFunctionPimps[In, Out](pf: In ~> Out) {
    def isUndefinedAt(in: In): Boolean = !pf.isDefinedAt(in)

    def partition[CC[A]](ins: GenTraversableLike[In, GenTraversable[In]])
      (implicit cbf: CCBF[Either[In, Out], CC], icbf: CCBF[In, CC], ocbf: CCBF[Out, CC]): (CC[In], CC[Out]) =
        ins.map(either).partitionEithers[CC](icbf, ocbf)

    def either:  In ⇒ Either[In, Out] = toRight
    def toRight: In ⇒ Either[In, Out] = (in: In) ⇒ pf.lift(in).toRight(in)
    def toLeft:  In ⇒ Either[Out, In] = (in: In) ⇒ pf.lift(in).toLeft(in)

    def first[C]:  (In, C) ~> (Out, C) = ***(identityPF[C])
    def second[C]: (C, In) ~> (C, Out) = identityPF[C] *** pf

    def &&&[Out2](rhs: In ~> Out2): In ~> (Out, Out2) = new (In ~> (Out, Out2)) {
      def isDefinedAt(in: In): Boolean = pf.isDefinedAt(in) && rhs.isDefinedAt(in)
      def apply(in: In): (Out, Out2) = (pf(in), rhs(in))
    }

    def ***[In2, Out2](rhs: In2 ~> Out2): (In, In2) ~> (Out, Out2) = new ((In, In2) ~> (Out, Out2)) {
      def isDefinedAt(in: (In, In2)): Boolean = pf.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
      def apply(in: (In, In2)): (Out, Out2) = (pf.apply(in._1), rhs.apply(in._2))
    }
  }

  implicit class PartialEndoFunctionPimps[A](val pf: A ~> A) extends AnyVal {
    def unify: A ⇒ A = (a: A) ⇒ pf.lift(a).getOrElse(a)
  }

  private class GuardedPartialFunction[A, B](p: Predicate[A], f: A ⇒ B) extends (A ~> B) {
    def isDefinedAt(a: A): Boolean = p(a)
    def apply(a: A): B = f(a)
  }

  def identityPF[A]: A ~> A = PartialFunction(identity[A])
  def equalC[A]: A ⇒ A ⇒ Boolean = (l: A) ⇒ (r: A) ⇒ l equals r
  def nand[A](ps: Predicate[A]*): Predicate[A] = and(ps: _*).not
  def nor[A](ps: Predicate[A]*): Predicate[A]  = or(ps: _*).not
  def or[A](ps: Predicate[A]*): Predicate[A]   = ps.foldLeft((a: A) ⇒ false)(_ or _)
  def and[A](ps: Predicate[A]*): Predicate[A]  = ps.foldLeft((a: A) ⇒ true)(_ and _)
}