package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.throwable._
import pimpathon.util._


class ThrowableTest {
  @Test def stackTraceAsString(): Unit = assertEquals(
    boom.toString :: boom.getStackTrace.toList.take(3).map("\tat " + _), boom.stackTraceAsString().lines.toList.take(4)
  )
}