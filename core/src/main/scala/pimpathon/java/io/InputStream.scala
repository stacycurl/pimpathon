package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec


object inputStream {
  implicit class RichInputStream(val is: InputStream) extends AnyVal {
    def read(os: OutputStream): Unit = {
      @tailrec def recurse(buf: Array[Byte]): Unit = {
        val len = is.read(buf)

        if (len > 0) {
          os.write(buf, 0, len)
          recurse(buf)
        }
      }

      recurse(new Array[Byte](8192))
    }
  }
}
