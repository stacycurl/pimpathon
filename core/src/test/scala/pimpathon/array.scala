package pimpathon

import _root_.java.nio.charset.Charset

import org.junit.Test

import org.junit.Assert._
import pimpathon.array._
import pimpathon.util._


class ArrayTest {
  @Test def toHex(): Unit = assertEquals("7e57ab1e", Array[Byte](126, 87, -85, 30).toHex)

  @Test def readUpToN(): Unit = {
    def read(input: String, n: Int, bufferSize: Int): (String, Int) = {
      val buffer = new Array[Byte](bufferSize)
      val count = buffer.readUpToN(n, createInputStream(input))

      (new String(buffer).substring(0, count), count)
    }

    on(9, 8, 7).calling(read("contents", _, 8)).produces(("contents", 8), ("contents", 8), ("content", 7))
  }

  @Test def copyUpToN(): Unit = {
    def copy(input: String, n: Int, bufferSize: Int): (String, Int) = {
      val buffer = new Array[Byte](bufferSize)
      val os = createOutputStream()
      val count = buffer.copyUpToN(n, createInputStream(input), os)

      (os.toString, count)
    }

    on(9, 8, 7).calling(copy("contents", _, 8)).produces(("contents", 8), ("contents", 8), ("content", 7))
  }

  @Test def copyTo(): Unit = assertEquals(
    List(0, 3, 4, 5, 0), Array(1, 2, 3, 4, 5).copyTo(2, Array(0, 0, 0, 0, 0), 1, 3).toList
  )

  @Test def asString(): Unit = {
    assertEquals("abc", Array('a'.toByte, 'b'.toByte, 'c'.toByte).asString(Charset.forName("UTF-8")))
    assertEquals("abc", Array('a'.toByte, 'b'.toByte, 'c'.toByte).asString)
  }
}