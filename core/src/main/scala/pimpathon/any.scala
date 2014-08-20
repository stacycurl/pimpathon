package pimpathon

import scala.util.Try

import pimpathon.function._


object any {
  implicit class AnyOps[A](val a: A) extends AnyVal {
    def calc[B](f: A => B): B = f(a)

    // These methods are aliased to suit individual preferences
    def tap(actions: (A => Unit)*): A        = { actions.foreach(action => action(a)); a }
    def update(action: A => Unit): A         = { action(a); a }
    def withSideEffect(action: A => Unit): A = { action(a); a }

    def tapIf(p: A => Boolean)(actions: (A => Unit)*): A     = if (p(a)) tap(actions: _*) else a
    def tapUnless(p: A => Boolean)(actions: (A => Unit)*): A = if (p(a)) a else tap(actions: _*)

    def attempt[B](f: A => B): Try[B] = Try(f(a))

    def partialMatch[B](pf: PartialFunction[A, B]): Option[B] = PartialFunction.condOpt(a)(pf)

    def lpair[B](f: A => B): (B, A) = (f(a), a)
    def rpair[B](f: A => B): (A, B) = (a, f(a))

    def filterSelf(p: A => Boolean): Option[A] = if (p(a)) Some(a) else None

    def withFinally[B](f: A => Unit)(t: A => B): B = try t(a) finally f(a)

    def cond[B](p: Predicate[A], ifTrue: A => B, ifFalse: A => B): B = if (p(a)) ifTrue(a) else ifFalse(a)
  }
}
