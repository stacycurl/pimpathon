package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.outputStream._


object inputStream extends InputStreamUtils(closeIn = true, closeOut = true)

case class InputStreamUtils(closeIn: Boolean, closeOut: Boolean, bufSize: Int = 8192) {
  implicit class InputStreamOps(val is: InputStream) {
    def read(os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): InputStream =
      is.tap(copy(_, os, closeIn, closeOut))

    def closeIf(condition: Boolean): InputStream = is.tapIf(_ => condition)(_.close)
  }

  def copy(
    is: InputStream, os: OutputStream,
    closeIn: Boolean = closeIn, closeOut: Boolean = closeOut, buf: Array[Byte] = new Array[Byte](bufSize)
  ): Unit = {
    @tailrec def recurse(): Unit = {
      val len = is.read(buf)

      if (len > 0) {
        os.write(buf, 0, len)
        recurse()
      }
    }

    recurse()

    if (closeIn)  is.close
    if (closeOut) os.close
  }
}
