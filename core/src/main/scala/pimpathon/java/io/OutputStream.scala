package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.inputStream._


object outputStream extends OutputStreamUtils(closeOut = true, closeIn = true)

class OutputStreamUtils(closeOut: Boolean, closeIn: Boolean) {
  implicit class OutputStreamOps[OS <: OutputStream](val os: OS) {
    def write(is: InputStream, closeOut: Boolean = closeOut, closeIn: Boolean = closeIn): OS =
      os.tap(is.read(_, closeIn, closeOut))

    def closeIf(condition: Boolean): OS     = os.tapIf(_ => condition)(_.close)
    def closeUnless(condition: Boolean): OS = os.tapUnless(_ => condition)(_.close)
  }
}
