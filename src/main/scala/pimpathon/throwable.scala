package pimpathon

import _root_.java.io.{PrintWriter, StringWriter}

import pimpathon.any._


object throwable {
  implicit class ThrowablePimps(val self: Throwable) extends AnyVal {
    def stackTraceAsString(): String =
      new StringWriter().tap(sw ⇒ self.printStackTrace(new PrintWriter(sw, true))).toString
  }
}