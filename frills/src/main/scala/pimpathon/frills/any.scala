package pimpathon.frills

import scalaz.{Failure, Success, Validation}

import pimpathon.function._


object any {
  implicit class AnyFrills[A](val a: A) extends AnyVal {
    def ensure[E](e: ⇒ E)(p: Predicate[A]): Validation[E, A] = if (p(a)) Success(a) else Failure(e)
  }
}