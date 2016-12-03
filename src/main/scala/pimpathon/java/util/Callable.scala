package pimpathon.java.util

import scala.language.implicitConversions

import java.util.concurrent.Callable
import scala.util.Try


object callable {
  implicit def callbackFromThunk[A](thunk: () ⇒ A): Callable[A] = create(thunk())

  def create[A](action: ⇒ A): Callable[A] = new Callable[A] {
    override def call(): A = action
  }

  implicit class CallablePimps[A](val self: Callable[A]) extends AnyVal {
    def attempt: Callable[Try[A]] = create(Try(self.call()))
  }
}