package pimpathon.java.util

import java.util.concurrent.Callable
import pimpathon.PSpec
import pimpathon.java.util.callable._

import scala.util.{Failure, Success}


class CallableSpec extends PSpec {
  "create" in call(callable.create(1)) ≡ 1

  "fromThunk" in call(() ⇒ 1) ≡ 1

  "attempt" in {
    on(callable.create(3), callable.create(goBoom)).calling(_.attempt.call).produces(Success(3), Failure(boom))

    assertThrows[InterruptedException]("fatal") {
      callable.create[Int](throw new InterruptedException("fatal")).attempt.call()
    }
  }

  private def call[A](callable: Callable[A]): A = callable.call()
}