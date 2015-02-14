package pimpathon.scalaz.std

import org.junit.Assert._
import org.junit.Test

import pimpathon.scalaz.std.option._

import scalaz.{NonEmptyList, Failure, Success}


class OptionTest {
  @Test def toSuccessNel(): Unit = {
    assertEquals(Success(1),                    Some(1).toSuccessNel("fail"))
    assertEquals(Failure(NonEmptyList("fail")), None.toSuccessNel("fail"))
  }
}