package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.array._


class ArrayTest {
  @Test def toHex: Unit = {
    assertEquals("7e57ab1e", Array[Byte](126, 87, -85, 30).toHex)
  }
}

