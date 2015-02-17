package pimpathon.scalaz.std

import scalaz.ValidationNel

import scalaz.syntax.std.option._


object option {
  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps[A](option)

  class OptionOps[A](option: Option[A]) {
    def toSuccessNel[E](e: ⇒ E): ValidationNel[E, A] = option.toSuccess(e).toValidationNel
  }
}