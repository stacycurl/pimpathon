package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.option._


class OptionTest {
  @Test def getOrThrow {
    assertEquals("missing", util.intercept[NoSuchElementException] {
      None.getOrThrow("missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      None.getOrThrow(new RuntimeException("missing"))
    }.getMessage)
  }
}
