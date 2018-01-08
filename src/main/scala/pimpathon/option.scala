package pimpathon

import scala.{PartialFunction ⇒ ~>}
import scala.util.Try


object option {
  implicit class OptionPimps[A](val self: Option[A]) extends AnyVal {
    def tapNone[Discarded](none: ⇒ Discarded): Option[A] = tap(none, _ ⇒ {})
    def tapSome[Discarded](some: A ⇒ Discarded): Option[A] = tap({}, some)
    def tap[Discarded](none: ⇒ Discarded, some: A ⇒ Discarded): Option[A] = { self.fold(none)(some); self }
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: ⇒ Exception): A = self.getOrElse(throw exception)
    def toTry: Try[A] = self.fold(pimpTry.failure[A](new NoSuchElementException))(pimpTry.success[A])
    def invert(a: A): Option[A] = self.fold(Some(a): Option[A])(_ ⇒ None)
    def amass[B](pf: A ~> Option[B]): Option[B] = self.flatMap(a ⇒ pf.lift(a).getOrElse(None))
    def toEither[L,R](none: => L, some: A => R): Either[L,R] = self.map(a => Right(some(a))).getOrElse(Left(none))
  }
}