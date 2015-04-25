package pimpathon

import scala.util.Try


object option {
  implicit class OptionPimps[A](val option: Option[A]) extends AnyVal {
    def tapNone[Discarded](none: ⇒ Discarded): Option[A] = tap(none, _ ⇒ {})
    def tapSome[Discarded](some: A ⇒ Discarded): Option[A] = tap({}, some)
    def tap[Discarded](none: ⇒ Discarded, some: A ⇒ Discarded): Option[A] = { option.fold(none)(some); option }
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: ⇒ Exception): A = option.getOrElse(throw exception)
    def toTry: Try[A] = option.fold(pimpTry.failure[A](new NoSuchElementException))(pimpTry.success[A])
    def invert(a: A): Option[A] = option.fold(Some(a): Option[A])(_ ⇒ None)
  }
}