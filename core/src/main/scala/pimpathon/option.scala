package pimpathon

import pimpathon.any.AnyOps


object option {
  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps[A](option)

  class OptionOps[A](option: Option[A]) {
    def tapNone(none: => Unit): Option[A] = tap(none, _ => {})
    def tapSome(some: A => Unit): Option[A] = tap({}, some)
    def tap(none: => Unit, some: A => Unit): Option[A] = new AnyOps(option).tap(_.map(some).getOrElse(none))
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: => Exception): A = option.getOrElse(throw exception)
  }
}