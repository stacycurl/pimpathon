package pimpathon

import _root_.java.math.BigInteger

import pimpathon.string._


object array {
  implicit def byteArrayOps(array: Array[Byte]): ByteArrayOps = new ByteArrayOps(array)

  class ByteArrayOps(array: Array[Byte]) {
    def toHex(length: Int): String = toHex.prefixPadTo(length, '0')
    def toHex: String = new BigInteger(1, array).toString(16)
  }
}

