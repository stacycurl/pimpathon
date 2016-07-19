package pimpathon

import _root_.java.io.{PrintWriter, StringWriter}

import pimpathon.any._


object throwable {
  implicit class ThrowablePimps(val value: Throwable) extends AnyVal {
    def stackTraceAsString(): String =
      new StringWriter().tap(sw ⇒ value.printStackTrace(new PrintWriter(sw, true))).toString
  }
}