package pimpathon

import org.junit.Test
import scala.util.Success
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.option._
import scalaz.std.option._


class OptionTest {
  @Test def tap(): Unit = {
    val strings = new M.ListBuffer[String]

    none[String].tap(strings += "none", strings += _)
    assertEquals(List("none"), strings.toList)

    some("some").tap(strings += "none", strings += _)
    assertEquals(List("none", "some"), strings.toList)
  }

  @Test def tapNone(): Unit = {
    val strings = new M.ListBuffer[String]

    none[String].tapNone(strings += "none")
    assertEquals(List("none"), strings.toList)

    some("some").tapNone(strings += "none")
    assertEquals(List("none"), strings.toList)
  }

  @Test def getOrThrow(): Unit = {
    assertEquals("present", Some("present").getOrThrow("missing"))
    assertEquals("present", Some("present").getOrThrow(new Exception("missing")))
    assertEquals("present", Some("present").getOrThrow(util.goBoom: Exception))

    assertEquals("missing", util.intercept[NoSuchElementException] {
      None.getOrThrow("missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      None.getOrThrow(new RuntimeException("missing"))
    }.getMessage)
  }

  @Test def toTry(): Unit = {
    assertEquals(Success[String](classOf[NoSuchElementException].getName),
      none[String].toTry.failed.map(_.getClass.getName))

    assertEquals(Success[String]("foo"), Some("foo").toTry)
  }
}