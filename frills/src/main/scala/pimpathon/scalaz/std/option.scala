package pimpathon.scalaz.std

import scalaz.ValidationNel

import scalaz.syntax.std.option._


object option {
  implicit class OptionFrills[A](val option: Option[A]) extends AnyVal {
    def toSuccessNel[E](e: ⇒ E): ValidationNel[E, A] = option.toSuccess(e).toValidationNel
    def toFailureNel[B](b: ⇒ B): ValidationNel[A, B] = option.toFailure(b).toValidationNel
  }
}