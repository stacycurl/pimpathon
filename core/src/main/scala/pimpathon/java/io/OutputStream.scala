package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.inputStream._


object outputStream extends OutputStreamUtils(closeOut = true, closeIn = true)

class OutputStreamUtils(closeOut: Boolean, closeIn: Boolean) {
  implicit class OutputStreamOps(val os: OutputStream) {
    def write(is: InputStream, closeOut: Boolean = closeOut, closeIn: Boolean = closeIn): OutputStream =
      os.tap(is.read(_, closeIn, closeOut))

    def closeIf(condition: Boolean): OutputStream = os.tapIf(_ => condition)(_.close)
  }
}
