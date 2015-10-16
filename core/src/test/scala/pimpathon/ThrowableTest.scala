package pimpathon

import org.junit.Test

import pimpathon.throwable._
import pimpathon.util._


class ThrowableTest {
  @Test def stackTraceAsString(): Unit = boom.stackTraceAsString().lines.toList.take(4) ===
    boom.toString :: boom.getStackTrace.toList.take(3).map("\tat " + _)
}