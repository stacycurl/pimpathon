package pimpathon.scalaz.std

import scalaz.ValidationNel

import scalaz.syntax.std.option._


object option {
  implicit class OptionFrills[A](val self: Option[A]) extends AnyVal {
    def toSuccessNel[E](e: ⇒ E): ValidationNel[E, A] = self.toSuccess(e).toValidationNel
    def toFailureNel[B](b: ⇒ B): ValidationNel[A, B] = self.toFailure(b).toValidationNel
  }
}