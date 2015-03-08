package pimpathon.java.util

import java.util.concurrent.Callable

import org.junit.Test

import org.junit.Assert._
import pimpathon.java.util.callable._
import pimpathon.util._


class CallableTest {
  @Test def create(): Unit = assertEquals(1, call(callable.create(1)))

  @Test def fromThunk(): Unit = assertEquals(1, call(() ⇒ 1))

  @Test def attempt(): Unit =
    on(callable.create(3), callable.create(goBoom)).calling(_.attempt.call).produces(Right(3), Left(boom))

  private def call[A](callable: Callable[A]): A = callable.call()
}