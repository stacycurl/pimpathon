package pimpathon

import scala.util.{Failure, Success, Try}


object option {
  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: => Exception): A = option.getOrElse(throw exception)
    def toTry: Try[A] = option.fold(pimpTry.failure[A](new NoSuchElementException))(pimpTry.success[A](_))
  }
}
