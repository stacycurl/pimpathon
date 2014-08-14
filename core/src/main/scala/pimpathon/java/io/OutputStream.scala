package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.inputStream._


object outputStream {
  implicit class OutputStreamOps(val os: OutputStream) extends AnyVal {
    def write(is: InputStream, closeOut: Boolean = true, closeIn: Boolean = true): OutputStream =
      os.tap(_ => is.read(os, closeIn, closeOut))

    def closeIf(condition: Boolean): OutputStream = os.tapIf(_ => condition)(_.close)
  }
}
