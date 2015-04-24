package pimpathon

import _root_.java.io.{PrintWriter, StringWriter}

import pimpathon.any._


object throwable {
  implicit def throwablePimps(throwable: Throwable): ThrowablePimps = new ThrowablePimps(throwable)
  
  class ThrowablePimps(value: Throwable) {
    def stackTraceAsString(): String =
      new StringWriter().tap(sw ⇒ value.printStackTrace(new PrintWriter(sw, true))).toString
  }
}