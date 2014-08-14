package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._


object inputStream {
  implicit class InputStreamOps(val is: InputStream) extends AnyVal {
    def read(os: OutputStream): InputStream = { copy(is, os); is }
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
