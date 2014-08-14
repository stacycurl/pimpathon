package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec


object inputStream {
  implicit def inputStreamOps(is: InputStream): InputStreamOps = new InputStreamOps(is)

  class InputStreamOps(is: InputStream) {
    def read(os: OutputStream): InputStream = { copy(is, os); is }
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
