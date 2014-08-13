package pimpathon

import org.junit.Test
import scala.util.{Failure, Success, Try}

import org.junit.Assert._
import pimpathon.option._
import scalaz.std.option._


class OptionTest {
  @Test def getOrThrow {
    assertEquals("missing", util.intercept[NoSuchElementException] {
      None.getOrThrow("missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      None.getOrThrow(new RuntimeException("missing"))
    }.getMessage)
  }

  @Test def toTry {
    assertEquals(Success[String](classOf[NoSuchElementException].getName),
      none[String].toTry.failed.map(_.getClass.getName))

    assertEquals(Success[String]("foo"), Some("foo").toTry)
  }
}
