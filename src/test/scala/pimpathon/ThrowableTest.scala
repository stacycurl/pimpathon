package pimpathon

import pimpathon.throwable._


class ThrowableSpec extends PSpec {
  "stackTraceAsString" in boom.stackTraceAsString().lines.toList.take(4) ≡
    (boom.toString :: boom.getStackTrace.toList.take(3).map("\tat " + _))
}