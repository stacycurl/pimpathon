package pimpathon.java.io

import java.io.{IOException, BufferedOutputStream, InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.java.io.inputStream._


object outputStream extends OutputStreamUtils(closeOut = true, closeIn = true, bufferSize = 8192)

case class OutputStreamUtils(closeOut: Boolean, closeIn: Boolean, bufferSize: Int) {
  implicit def outputStreamOps[OS <: OutputStream](os: OS): OutputStreamOps[OS] = new OutputStreamOps[OS](os)

  class OutputStreamOps[OS <: OutputStream](os: OS) {
    def drain(is: InputStream, closeOut: Boolean = closeOut, closeIn: Boolean = closeIn): OS =
      os.tap(is.drain(_, closeIn, closeOut))

    def <<(is: InputStream): OS = drain(is, closeOut = false, closeIn = false)

    def closeAfter[A](f: OS => A): A        = os.withFinally(_.attemptClose())(f)
    def closeIf(condition: Boolean): OS     = os.tapIf(_ => condition)(_.close())
    def closeUnless(condition: Boolean): OS = os.tapUnless(_ => condition)(_.close())
    def attemptClose(): Either[Throwable, Unit] = os.attempt(_.close())

    def buffered: BufferedOutputStream = new BufferedOutputStream(os, bufferSize)

    def writeN(is: InputStream, n: Long): OS = os.tap(_.writeUpToN(is, n).calc(count => if (count != n)
      throw new IOException("Failed to write %d only %d were available".format(n, count))
    ))

    def writeUpToN(is: InputStream, n: Long): Long = is.readUpToN(os, n)
  }
}
