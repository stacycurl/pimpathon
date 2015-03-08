package pimpathon.java.util

import java.util.concurrent.Callable

import pimpathon.any._


object callable {
  implicit def callablePimps[A](value: Callable[A]): CallablePimps[A] = new CallablePimps[A](value)

  implicit def callbackFromThunk[A](thunk: () ⇒ A): Callable[A] = create(thunk())

  def create[A](action: ⇒ A): Callable[A] = new Callable[A] {
    override def call(): A = action
  }

  class CallablePimps[A](value: Callable[A]) {
    def attempt: Callable[Either[Throwable, A]] = create(new AnyPimps(value).attempt(_.call()))
  }
}