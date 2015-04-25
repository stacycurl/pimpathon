package pimpathon

import scala.util.{Failure, Success, Try}


object pimpTry {
  implicit class TryPimps[A](val value: Try[A]) extends AnyVal {
    def getMessage: Option[String] = fold(t ⇒ Some(t.getMessage), _ ⇒ None)
    def toEither: Either[Throwable, A] = fold(Left(_), Right(_))

    private def fold[B](failure: Throwable ⇒ B, success: A ⇒ B): B = value match {
      case Success(a) ⇒ success(a)
      case Failure(t) ⇒ failure(t)
    }
  }

  def failure[A](throwable: Throwable): Try[A] = Failure[A](throwable)
  def success[A](a: A): Try[A] = Success[A](a)
}