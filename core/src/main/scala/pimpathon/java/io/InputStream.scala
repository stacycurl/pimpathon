package pimpathon.java.io

import java.io.{BufferedInputStream, InputStream, OutputStream}
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
