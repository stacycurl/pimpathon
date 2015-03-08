package pimpathon

import pimpathon.any.AnyPimps


object option {
  implicit def optionPimps[A](option: Option[A]): OptionPimps[A] = new OptionPimps[A](option)

  class OptionPimps[A](option: Option[A]) {
    def tapNone(none: ⇒ Unit): Option[A] = tap(none, _ ⇒ {})
    def tapSome(some: A ⇒ Unit): Option[A] = tap({}, some)
    def tap(none: ⇒ Unit, some: A ⇒ Unit): Option[A] = new AnyPimps(option).tap(_.map(some).getOrElse(none))
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: ⇒ Exception): A = option.getOrElse(throw exception)
    def invert(a: A): Option[A] = option.map(_ ⇒ None: Option[A]).getOrElse(Some(a))
  }
}