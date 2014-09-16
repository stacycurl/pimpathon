package pimpathon.java.io

import java.io.{BufferedInputStream, IOException, InputStream, OutputStream}
import scala.annotation.tailrec
import scala.util.Try

import pimpathon.any._
import pimpathon.java.io.outputStream._


object inputStream extends InputStreamUtils(closeIn = true, closeOut = true, bufferSize = 8192)

case class InputStreamUtils(closeIn: Boolean, closeOut: Boolean, bufferSize: Int) {
  implicit class InputStreamOps[IS <: InputStream](val is: IS) {
    def drain(os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): IS =
      is.tap(copy(_, os, closeIn, closeOut))

    def >>(os: OutputStream): IS = drain(os, closeIn = false, closeOut = false)

    def attemptClose(): Try[Unit] = Try(is.close())
    def closeAfter[A](f: IS => A): A        = is.withFinally(_.attemptClose())(f)
    def closeIf(condition: Boolean): IS     = is.tapIf(_ => condition)(_.close())
    def closeUnless(condition: Boolean): IS = is.tapUnless(_ => condition)(_.close())

    def buffered: BufferedInputStream = new BufferedInputStream(is, bufferSize)

    def readN(os: OutputStream, n: Long) : IS = is.tap {
     i =>
      val count = i.readUpToN(os, n)
      if(count != n)
        throw new IOException("Failed to read " + n + " bytes, only " + count + " were available")
    }

    def readUpToN(os: OutputStream, limit: Long) : Long = {
      require(limit >= 0, "You can't read a negative number of bytes!")
      val buffer: Array[Byte] = new Array[Byte](bufferSize)

      @tailrec def recurse(count : Long) : Long = {
        if (count == limit) count else {
          val written = is.read(buffer, 0, scala.math.min(limit - count, bufferSize).toInt)

          if (written == -1) count else {
            os.write(buffer, 0, written)
            recurse(count + written)
          }
        }
      }

      recurse(0)
    }

  }

  def copy(
    is: InputStream, os: OutputStream,
    closeIn: Boolean = closeIn, closeOut: Boolean = closeOut,
    buffer: Array[Byte] = new Array[Byte](bufferSize)
  ): Unit = {
    try Iterator.continually(is.read(buffer)).takeWhile(_ > 0).foreach(os.write(buffer, 0, _))
    finally {
      if (closeIn)  is.attemptClose()
      if (closeOut) os.attemptClose()
    }
  }
}
