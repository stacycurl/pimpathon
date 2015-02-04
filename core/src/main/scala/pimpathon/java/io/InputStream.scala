package pimpathon.java.io

import scala.language.implicitConversions

import java.io.{BufferedInputStream, IOException, InputStream, OutputStream}
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.util.Try

import pimpathon.any._
import pimpathon.array._


object inputStream extends InputStreamUtils()

case class InputStreamUtils(closeIn: Boolean = true, closeOut: Boolean = true, bufferSize: Int = 8192) {
  implicit def inputStreamOps[IS <: InputStream](is: IS): InputStreamOps[IS] =
    new InputStreamOps[IS](is, this)

  def copy(is: InputStream, os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): Unit = {
    withBuffer(buffer => {
      try Iterator.continually(is.read(buffer)).takeWhile(_ > 0).foreach(os.write(buffer, 0, _))
      finally {
        if (closeIn)  is.attemptClose()
        if (closeOut) os.attemptClose()
      }
    })
  }

  private[io] def withBuffer[A](f: Array[Byte] => A): A = f(new Array[Byte](bufferSize))
}

class InputStreamOps[IS <: InputStream](is: IS, utils: InputStreamUtils) {
  import utils._

  def >>(os: OutputStream): IS = drain(os, closeIn = false, closeOut = false)

  def drain(os: OutputStream, closeIn: Boolean = closeIn, closeOut: Boolean = closeOut): IS =
    is.tap(copy(_, os, closeIn, closeOut))

  def closeAfter[A](f: IS => A): A        = is.withFinally(_.attemptClose())(f)
  def closeIf(condition: Boolean): IS     = is.tapIf(_ => condition)(_.close())
  def closeUnless(condition: Boolean): IS = is.tapUnless(_ => condition)(_.close())
  def attemptClose(): Try[Unit] = Try(is.close())

  def buffered: BufferedInputStream = new BufferedInputStream(is, bufferSize)
  def gunzip: GZIPInputStream = new GZIPInputStream(is, bufferSize)

  def readN(os: OutputStream, n: Long): IS = is.tap(_.readUpToN(os, n) |> (count => if (count != n)
    throw new IOException(s"Failed to read $n bytes, only $count were available")
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