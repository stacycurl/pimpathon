package pimpathon.scalaz.std

import org.junit.Test
import scalaz.{NonEmptyList ⇒ NEL, Failure, Success}

import pimpathon.scalaz.std.option._


class OptionTest {
  @Test def toSuccessNel(): Unit = calling(_.toSuccessNel("fail")).produces(Success(1), Failure(NEL("fail")))
  @Test def toFailureNel(): Unit = calling(_.toFailureNel("succeed")).produces(Failure(NEL(1)), Success("succeed"))

  private def calling[A](f: Option[Int] ⇒ A) = pimpathon.util.on(Some(1), None).calling(f)
}