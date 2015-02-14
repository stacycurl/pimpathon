package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.option._
import pimpathon.util._
import scalaz.std.option._


class OptionTest {
  @Test def tap(): Unit = {
    assertEquals(List("none"), strings.run(ss => none[String].tap(ss += "none", ss += _)))
    assertEquals(List("some"), strings.run(ss => some("some").tap(ss += "none", ss += _)))
  }

  @Test def tapNone(): Unit = {
    assertEquals(List("none"), strings.run(ss => none[String].tapNone(ss += "none")))
    assertEquals(Nil,          strings.run(ss => some("some").tapNone(ss += "none")))
  }

  @Test def tapSome(): Unit = {
    assertEquals(Nil,          strings.run(ss => none[String].tapSome(ss += _)))
    assertEquals(List("some"), strings.run(ss => some("some").tapSome(ss += _)))
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
}