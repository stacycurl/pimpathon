package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.function._


object any {
  implicit def anyOps[A](a: A): AnyOps[A] = new AnyOps[A](a)

  class AnyOps[A](a: A) {
    def calc[B](f: A ⇒ B): B = f(a)
    def |>[B](f: A ⇒ B): B = f(a)
    def calcIf[B](p: Predicate[A])(f: A ⇒ B): Option[B] = if (p(a)) Some(f(a)) else None
    def calcPF[B](pf: PartialFunction[A, B]): Option[B] = pf.lift(a)
    def transform(pf: PartialFunction[A, A]): A = pf.unify(a)

    def tapIf(p: Predicate[A])(actions: (A ⇒ Unit)*): A     = if (p(a)) tap(actions: _*) else a
    def tapUnless(p: Predicate[A])(actions: (A ⇒ Unit)*): A = if (p(a)) a else tap(actions: _*)

    def tapPF[Discarded](action: PartialFunction[A, Discarded]): A = { action.lift(a); a }

    def attempt[B](f: A ⇒ B): Either[Throwable, B] = try { Right(f(a)) } catch { case t: Throwable ⇒ Left(t) }

    def partialMatch[B](pf: PartialFunction[A, B]): Option[B] = PartialFunction.condOpt(a)(pf)

    def lpair[B](f: A ⇒ B): (B, A) = (f(a), a)
    def rpair[B](f: A ⇒ B): (A, B) = (a, f(a))

    def filterSelf(p: Predicate[A]): Option[A] = if (p(a)) Some(a) else None
    def ifSelf(p: Predicate[A]): Option[A] = if (p(a)) Some(a) else None

    def filterNotSelf(p: Predicate[A]): Option[A] = if (p(a)) None else Some(a)
    def unlessSelf(p: Predicate[A]): Option[A] = if (p(a)) None else Some(a)

    def passes: AnyCapturer[A] = new AnyCapturer[A](a, b ⇒ if (b) Some(a) else None)
    def fails: AnyCapturer[A]  = new AnyCapturer[A](a, b ⇒ if (b) None else Some(a))

    def withFinally[B](f: A ⇒ Unit)(t: A ⇒ B): B = try t(a) finally f(a)

    def cond[B](p: Predicate[A], ifTrue: A ⇒ B, ifFalse: A ⇒ B): B = if (p(a)) ifTrue(a) else ifFalse(a)

    def addTo[To](builder: M.Builder[A, To]): A = tap(builder += _)

    def unfold[B](f: A ⇒ Option[(B, A)]): Stream[B] = f(a) match {
      case None            ⇒ Stream.empty[B]
      case Some((b, newA)) ⇒ Stream.cons(b, newA.unfold(f))
    }

    // These methods are aliased to suit individual preferences
    def update[Discarded](actions: (A ⇒ Discarded)*): A         = tap(actions: _*)
    def withSideEffect[Discarded](actions: (A ⇒ Discarded)*): A = tap(actions: _*)
    def tap[Discarded](actions: (A ⇒ Discarded)*): A            = { actions.foreach(action ⇒ action(a)); a }
  }

  class AnyCapturer[A](a: A, andThen: Boolean ⇒ Option[A]) {
    def one(disjuncts: Predicate[A]*): Option[A]  = andThen(function.or(disjuncts: _*).apply(a))
    def all(conjuncts: Predicate[A]*): Option[A]  = andThen(function.and(conjuncts: _*).apply(a))
    def none(conjuncts: Predicate[A]*): Option[A] = andThen(function.nand(conjuncts: _*).apply(a))
    def some(disjuncts: Predicate[A]*): Option[A] = andThen(function.nor(disjuncts: _*).apply(a))
  }
}