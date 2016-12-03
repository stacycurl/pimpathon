package pimpathon

import _root_.java.io.{InputStream, OutputStream}
import _root_.java.math.BigInteger
import _root_.java.nio.charset.Charset

import pimpathon.any._
import pimpathon.string._


object array {
  implicit class ArrayPimps[A](val self: Array[A]) extends AnyVal {
    def copyTo(srcPos: Int, dest: Array[A], destPos: Int, length: Int): Array[A] =
      dest.tap(_ ⇒ System.arraycopy(self, srcPos, dest, destPos, length))
  }

  implicit class ByteArrayPimps(val self: Array[Byte]) extends AnyVal {
    def toHex(length: Int): String = toHex.prefixPadTo(length, '0')
    def toHex: String = new BigInteger(1, self).toString(16)

    def copyUpToN(n: Long, is: InputStream, os: OutputStream): Int =
      readUpToN(n, is).tapUnless(_ == -1)(os.write(self, 0, _))

    def readUpToN(n: Long, is: InputStream): Int =
      if (n == 0) -1 else is.read(self, 0, math.min(n, self.length).toInt)

    def asString: String = new String(self, Charset.forName("UTF-8"))
    def asString(charset: Charset): String = new String(self, charset)
  }
}