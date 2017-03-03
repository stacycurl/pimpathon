package pimpathon

import scala.language.higherKinds
import scala.runtime.AbstractPartialFunction

import scala.{PartialFunction ⇒ ~>}
import scala.collection.{GenTraversable, GenTraversableLike}

import pimpathon.boolean._
import pimpathon.genTraversableLike._

import scala.util.Try


object function {
  type Predicate[-A] = A ⇒ Boolean

  implicit class FunctionPimps[A, B](val self: A ⇒ B) extends AnyVal {
    def attempt: A ⇒ Try[B] = a ⇒ Try(self(a))
    def guardWith(p: Predicate[A]): A ~> B = p guard self
  }

  implicit class FunctionOptionPimps[A, B](val self: A ⇒ Option[B]) extends AnyVal {
    def unlift: A ~> B = new AbstractPartialFunction[A, B] { // Gee thanks for making PF.Lifted & Unlifted private
      def isDefinedAt(a: A): Boolean = self(a).isDefined
      override def applyOrElse[A1 <: A, B1 >: B](a: A1, default: (A1) ⇒ B1): B1 = self(a).getOrElse(default(a))
      override def lift: (A) ⇒ Option[B] = self
    }
  }

  implicit class CurriedFunction2Pimps[A, B, C](val self: A ⇒ B ⇒ C) extends AnyVal {
    def tupled: ((A, B)) ⇒ C = Function.uncurried(self).tupled
  }

  implicit class PredicatePimps[A](val self: Predicate[A]) extends AnyVal {
    def cond[B](ifTrue: ⇒ B, ifFalse: ⇒ B): A ⇒ B = a ⇒ self(a).cond(ifTrue, ifFalse)

    def and(q: Predicate[A]): Predicate[A] = (a: A) ⇒ self(a) && q(a)
    def or(q: Predicate[A]):  Predicate[A] = (a: A) ⇒ self(a) || q(a)
    def not:                  Predicate[A] = (a: A) ⇒ !self(a)

    def exists: Predicate[List[A]] = _.exists(self)
    def forall: Predicate[List[A]] = _.forall(self)

    def ifSome: Predicate[Option[A]] = _.exists(self)

    def first[B]: Predicate[(A, B)] = (ab: (A, B)) => self(ab._1)

    def guard[B](f: A ⇒ B): A ~> B = new GuardedPartialFunction[A, B](self, f)
  }

  implicit class PartialFunctionPimps[In, Out](self: In ~> Out) {
    def isUndefinedAt(in: In): Boolean = !self.isDefinedAt(in)

    def partition[CC[A]](ins: GenTraversableLike[In, GenTraversable[In]])
      (implicit cbf: CCBF[Either[In, Out], CC], icbf: CCBF[In, CC], ocbf: CCBF[Out, CC]): (CC[In], CC[Out]) =
        ins.map(either).partitionEithers[CC](icbf, ocbf)

    def map[Out2](f: Out ⇒ Out2): In ~> Out2 = new (In ~> Out2) {
      def isDefinedAt(in: In): Boolean = self.isDefinedAt(in)
      def apply(in: In): Out2 = f(self(in))
    }

    def contramap[In2](f: In2 ⇒ In): In2 ~> Out = new (In2 ~> Out) {
      def isDefinedAt(in2: In2): Boolean = self.isDefinedAt(f(in2))
      def apply(in2: In2): Out = self(f(in2))
    }

    def either:  In ⇒ Either[In, Out] = toRight
    def toRight: In ⇒ Either[In, Out] = (in: In) ⇒ self.lift(in).toRight(in)
    def toLeft:  In ⇒ Either[Out, In] = (in: In) ⇒ self.lift(in).toLeft(in)

    def first[C]:  (In, C) ~> (Out, C) = ***(identityPF[C])
    def second[C]: (C, In) ~> (C, Out) = identityPF[C] *** self

    def &&&[Out2](rhs: In ~> Out2): In ~> (Out, Out2) = new (In ~> (Out, Out2)) {
      def isDefinedAt(in: In): Boolean = self.isDefinedAt(in) && rhs.isDefinedAt(in)
      def apply(in: In): (Out, Out2) = (self(in), rhs(in))
    }

    def ***[In2, Out2](rhs: In2 ~> Out2): (In, In2) ~> (Out, Out2) = new ((In, In2) ~> (Out, Out2)) {
      def isDefinedAt(in: (In, In2)): Boolean = self.isDefinedAt(in._1) && rhs.isDefinedAt(in._2)
      def apply(in: (In, In2)): (Out, Out2) = (self.apply(in._1), rhs.apply(in._2))
    }

    def |||[In2](rhs: In2 ~> Out): Either[In, In2] ~> Out = new (Either[In, In2] ~> Out) {
      def isDefinedAt(in: Either[In, In2]): Boolean = in.fold(self.isDefinedAt, rhs.isDefinedAt)
      def apply(in: Either[In, In2]): Out = in.fold(self.apply, rhs.apply)
    }
  }

  implicit class PartialEndoFunctionPimps[A](val self: A ~> A) extends AnyVal {
    def unify: A ⇒ A = (a: A) ⇒ self.lift(a).getOrElse(a)
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