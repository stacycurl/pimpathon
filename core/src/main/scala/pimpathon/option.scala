package pimpathon

import scala.util.Try

import pimpathon.any.AnyOps


object option {
  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def tapNone(none: ⇒ Unit): Option[A] = tap(none, _ ⇒ {})
    def tapSome(some: A ⇒ Unit): Option[A] = tap({}, some)
    def tap(none: ⇒ Unit, some: A ⇒ Unit): Option[A] = new AnyOps(option).tap(_.fold(none)(some))
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: ⇒ Exception): A = option.getOrElse(throw exception)
    def toTry: Try[A] = option.fold(pimpTry.failure[A](new NoSuchElementException))(pimpTry.success[A])
    def invert(a: A): Option[A] = option.fold(Some(a): Option[A])(_ ⇒ None)
  }
}