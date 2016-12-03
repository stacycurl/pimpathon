package pimpathon.frills

import scalaz.{Failure, NonEmptyList, Success, Validation, ValidationNel}

import pimpathon.function._


object any {
  implicit class AnyFrills[A](val self: A) extends AnyVal {
    def ensure[E](e: ⇒ E)(p: Predicate[A]): Validation[E, A] = if (p(self)) Success(self) else Failure(e)
    def ensureNel[E](e: ⇒ E)(p: Predicate[A]): ValidationNel[E, A] = if (p(self)) Success(self) else Failure(NonEmptyList(e))
  }
}