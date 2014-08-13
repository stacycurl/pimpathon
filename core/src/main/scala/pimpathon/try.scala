package pimpathon

import scala.util.{Failure, Success, Try}


object pimpTry {
  implicit class TryOps[A](val value: Try[A]) extends AnyVal {
    def toEither: Either[Throwable, A] = value match {
      case Success(a) => Right[Throwable, A](a)
      case Failure(t) => Left[Throwable, A](t)
    }
  }

  def failure[A](throwable: Throwable): Try[A] = Failure[A](throwable)
  def success[A](a: A): Try[A] = Success[A](a)
}

