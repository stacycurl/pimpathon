package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.inputStream._


object outputStream extends OutputStreamUtils(closeOut = true, closeIn = true)

class OutputStreamUtils(closeOut: Boolean, closeIn: Boolean) {
  implicit def outputStreamOps[OS <: OutputStream](os: OS): OutputStreamOps[OS] = new OutputStreamOps[OS](os)

  class OutputStreamOps[OS <: OutputStream](os: OS) {
    def drain(is: InputStream, closeOut: Boolean = closeOut, closeIn: Boolean = closeIn): OS =
      os.tap(is.drain(_, closeIn, closeOut))

    def attemptClose(): Either[Throwable, Unit] = os.attempt(_.close)
    def closeIf(condition: Boolean): OS     = os.tapIf(_ => condition)(_.close)
    def closeUnless(condition: Boolean): OS = os.tapUnless(_ => condition)(_.close)
  }
}
