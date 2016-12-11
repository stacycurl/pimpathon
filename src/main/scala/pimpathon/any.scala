package pimpathon

import scala.{PartialFunction ⇒ ~>}
import scala.collection.generic.{Growable, Shrinkable}
import scala.util.Try
import pimpathon.boolean._
import pimpathon.function._

import scala.reflect.ClassTag


object any {
  implicit class AnyPimps[A](val self: A) extends AnyVal {
    def calc[B](f: A ⇒ B): B = f(self)
    def |>[B](f: A ⇒ B): B = f(self)
    def calcIf[B](p: Predicate[A])(f: A ⇒ B): Option[B] = p(self).option(f(self))
    def calcUnless[B](p: Predicate[A])(f: A ⇒ B): Option[B] = (!p(self)).option(f(self))
    def calcPF[B](pf: A ~> B): Option[B] = pf.lift(self)
    def transform(pf: A ~> A): A = pf.unify(self)

    def tapIf[Discarded](p: Predicate[A])(actions: (A ⇒ Discarded)*): A     = if (p(self)) tap(actions: _*) else self
    def tapUnless[Discarded](p: Predicate[A])(actions: (A ⇒ Discarded)*): A = if (p(self)) self else tap(actions: _*)

    def tapPF[Discarded](action: A ~> Discarded): A = { action.lift(self); self }

    def castTo[B](implicit tag: ClassTag[B]): Option[B] =
      if (tag.runtimeClass.isAssignableFrom(self.getClass)) Some(self.asInstanceOf[B]) else None

    def attempt[B](f: A ⇒ B): Try[B] = Try(f(self))

    def partialMatch[B](pf: A ~> B): Option[B] = PartialFunction.condOpt(self)(pf)

    def lpair[B](f: A ⇒ B): (B, A) = (f(self), self)
    def rpair[B](f: A ⇒ B): (A, B) = (self, f(self))

    def filterSelf(p: Predicate[A]): Option[A] = p(self).option(self)
    def ifSelf(p: Predicate[A]): Option[A] = p(self).option(self)

    def filterNotSelf(p: Predicate[A]): Option[A] = (!p(self)).option(self)
    def unlessSelf(p: Predicate[A]): Option[A] = (!p(self)).option(self)

    def isOneOf(as: A*): Boolean = as.contains(self)
    def isNotOneOf(as: A*): Boolean = !as.contains(self)

    def containedIn(s: Set[A]): Boolean = s.contains(self)
    def notContainedIn(s: Set[A]): Boolean = !s.contains(self)

    def passes: AnyCapturer[A] = new AnyCapturer[A](self, b ⇒ b.option(self))
    def fails: AnyCapturer[A]  = new AnyCapturer[A](self, b ⇒ (!b).option(self))

    def withFinally[B](f: A ⇒ Unit)(t: A ⇒ B): B = try t(self) finally f(self)
    def tryFinally[B](t: A ⇒ B)(f: A ⇒ Unit): B = try t(self) finally f(self)

    def cond[B](p: Predicate[A], ifTrue: A ⇒ B, ifFalse: A ⇒ B): B = if (p(self)) ifTrue(self) else ifFalse(self)

    def addTo(as: Growable[A]): A = tap(as += _)
    def removeFrom(as: Shrinkable[A]): A = tap(as -= _)

    def unfold[B](f: A ⇒ Option[(B, A)]): Stream[B] = f(self).fold(Stream.empty[B])(ba ⇒ ba._1 #:: ba._2.unfold(f))

    // These methods are aliased to suit individual preferences
    def update[Discarded](actions: (A ⇒ Discarded)*): A         = tap(actions: _*)
    def withSideEffect[Discarded](actions: (A ⇒ Discarded)*): A = tap(actions: _*)
    def tap[Discarded](actions: (A ⇒ Discarded)*): A            = { actions.foreach(action ⇒ action(self)); self }

    def bounded(lower: A, upper: A)(implicit na: Numeric[A]): A = na.min(na.max(lower, self), upper)
  }

  class AnyCapturer[A](a: A, andThen: Boolean ⇒ Option[A]) {
    def one(disjuncts: Predicate[A]*): Option[A]  = andThen(function.or(disjuncts: _*).apply(a))
    def all(conjuncts: Predicate[A]*): Option[A]  = andThen(function.and(conjuncts: _*).apply(a))
    def none(conjuncts: Predicate[A]*): Option[A] = andThen(function.nand(conjuncts: _*).apply(a))
    def some(disjuncts: Predicate[A]*): Option[A] = andThen(function.nor(disjuncts: _*).apply(a))
  }
}