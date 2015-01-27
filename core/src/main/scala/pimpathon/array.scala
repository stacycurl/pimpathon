package pimpathon

import _root_.java.io.{InputStream, OutputStream}
import _root_.java.math.BigInteger

import pimpathon.any._
import pimpathon.string._


object array {
  implicit def arrayOps[A](array: Array[A]): ArrayOps[A] = new ArrayOps[A](array)
  implicit def byteArrayOps(array: Array[Byte]): ByteArrayOps = new ByteArrayOps(array)

  class ArrayOps[A](array: Array[A]) {
    def copyTo(srcPos: Int, dest: Array[A], destPos: Int, length: Int): Array[A] =
      dest.tap(_ => System.arraycopy(array, srcPos, dest, destPos, length))
  }

  class ByteArrayOps(array: Array[Byte]) {
    def toHex(length: Int): String = toHex.prefixPadTo(length, '0')
    def toHex: String = new BigInteger(1, array).toString(16)

    def copyUpToN(n: Long, is: InputStream, os: OutputStream): Int =
      readUpToN(n, is).tapUnless(_ == -1)(os.write(array, 0, _))

    def readUpToN(n: Long, is: InputStream): Int =
      if (n == 0) -1 else is.read(array, 0, math.min(n, array.length).toInt)
  }
}

