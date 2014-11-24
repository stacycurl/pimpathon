package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.option._


class OptionTest {
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
