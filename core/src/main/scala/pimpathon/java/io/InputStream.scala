package pimpathon.java.io

import java.io.{BufferedInputStream, IOException, InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.array._
import pimpathon.java.io.outputStream._


object inputStream extends InputStreamUtils()

case class InputStreamUtils(closeIn: Boolean = true, closeOut: Boolean = true, bufferSize: Int = 8192) {
  implicit def inputStreamOps[IS <: InputStream](is: IS): InputStreamOps[IS] = new InputStreamOps[IS](is)

  class InputStreamOps[IS <: InputStream](is: IS) {
    def drain(os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): IS =
      is.tap(copy(_, os, closeIn, closeOut))

    def >>(os: OutputStream): IS = drain(os, closeIn = false, closeOut = false)

    def attemptClose(): Either[Throwable, Unit] = is.attempt(_.close())
    def closeAfter[A](f: IS => A): A        = is.withFinally(_.attemptClose())(f)
    def closeIf(condition: Boolean): IS     = is.tapIf(_ => condition)(_.close())
    def closeUnless(condition: Boolean): IS = is.tapUnless(_ => condition)(_.close())

    def buffered: BufferedInputStream = new BufferedInputStream(is, bufferSize)

    def readN(os: OutputStream, n: Long): IS = is.tap(_.readUpToN(os, n).calc(count => if (count != n)
      throw new IOException("Failed to read %d bytes, only %d were available".format(n, count))
    ))

    def readUpToN(os: OutputStream, n: Long): Long = withBuffer(buffer => {
      require(n >= 0, "You can't read a negative number of bytes!")

      @tailrec def recurse(count: Long): Long = {
        val written = buffer.copyUpToN(n - count, is, os)

        if (written == -1) count else recurse(count + written)
      }

      recurse(0)
    })

  }

  def copy(is: InputStream, os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): Unit = {
    withBuffer(buffer => {
      try Iterator.continually(is.read(buffer)).takeWhile(_ > 0).foreach(os.write(buffer, 0, _))
      finally {
        if (closeIn)  is.attemptClose()
        if (closeOut) os.attemptClose()
      }
    })
  }

  private def withBuffer[A](f: Array[Byte] => A): A = new Array[Byte](bufferSize).calc(f)
}
