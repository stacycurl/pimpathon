package pimpathon

import scala.util.Try

import pimpathon.any.AnyOps


object option {
  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def tap(none: => Unit, some: A => Unit): Option[A] = new AnyOps(option).tap(_.fold(none)(some))
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: => Exception): A = option.getOrElse(throw exception)
    def toTry: Try[A] = option.fold(pimpTry.failure[A](new NoSuchElementException))(pimpTry.success[A])
  }
}