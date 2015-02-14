package pimpathon.scalaz.std

import scalaz.ValidationNel

import scalaz.syntax.std.option._


object option {
  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def toSuccessNel[E](e: => E): ValidationNel[E, A] = option.toSuccess(e).toValidationNel
  }
}