package pimpathon.java.util

import java.util.concurrent.Callable

import org.junit.Test

import org.junit.Assert._
import pimpathon.java.util.callable._
import pimpathon.util._

import scala.util.{Failure, Success}


class CallableTest {
  @Test def create(): Unit = assertEquals(1, call(callable.create(1)))

  @Test def fromThunk(): Unit = assertEquals(1, call(() ⇒ 1))

  @Test def attempt(): Unit = {
    on(callable.create(3), callable.create(goBoom)).calling(_.attempt.call).produces(Success(3), Failure(boom))

    assertThrows[InterruptedException]("fatal") {
      callable.create[Int](throw new InterruptedException("fatal")).attempt.call()
    }
  }

  private def call[A](callable: Callable[A]): A = callable.call()
}