package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.outputStream._


object inputStream {
  implicit def inputStreamOps(is: InputStream): InputStreamOps = new InputStreamOps(is)

  class InputStreamOps(is: InputStream) {
    def read(os: OutputStream, closeIn: Boolean = true, closeOut: Boolean = true): InputStream =
      is.tap(_ => os.tap(_ => copy(is, os)).closeIf(closeOut)).closeIf(closeIn)

    def closeIf(condition: Boolean): InputStream = is.tapIf(_ => condition)(_.close)
  }

  def copy(is: InputStream, os: OutputStream, buf: Array[Byte] = new Array[Byte](8192)): Unit = {
    @tailrec def recurse(): Unit = {
      val len = is.read(buf)

      if (len > 0) {
        os.write(buf, 0, len)
        recurse()
      }
    }

    recurse()
  }
}
