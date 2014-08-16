package pimpathon

import _root_.java.math.BigInteger

import pimpathon.string._


object array {
  implicit class ByteArrayOps(val array: Array[Byte]) extends AnyVal {
    def toHex(length: Int): String = toHex.prefixPadTo(length, '0')
    def toHex: String = new BigInteger(1, array).toString(16)
  }
}

