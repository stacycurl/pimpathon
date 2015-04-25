package pimpathon

import scala.util.{Failure, Success, Try}


object pimpTry {
  implicit class TryPimps[A](val value: Try[A]) extends AnyVal {
    def getMessage: Option[String] = fold(_ ⇒ None, t ⇒ Some(t.getMessage))
    def toEither: Either[Throwable, A] = fold(Right(_), Left(_))

    private def fold[B](success: A ⇒ B, failure: Throwable ⇒ B): B = value match {
      case Success(a) ⇒ success(a)
      case Failure(t) ⇒ failure(t)
    }
  }

  def failure[A](throwable: Throwable): Try[A] = Failure[A](throwable)
  def success[A](a: A): Try[A] = Success[A](a)
}