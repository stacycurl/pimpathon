package pimpathon.scalaz.std

import scalaz.ValidationNel

import scalaz.syntax.std.option._


object option {
  implicit def optionFrills[A](option: Option[A]): OptionFrills[A] = new OptionFrills[A](option)

  class OptionFrills[A](option: Option[A]) {
    def toSuccessNel[E](e: ⇒ E): ValidationNel[E, A] = option.toSuccess(e).toValidationNel
    def toFailureNel[B](b: ⇒ B): ValidationNel[A, B] = option.toFailure(b).toValidationNel
  }
}