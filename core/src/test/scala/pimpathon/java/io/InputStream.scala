package pimpathon.java.io

import java.io._
import org.junit.Test
import scala.util.{Failure, Success}

import org.junit.Assert._
import pimpathon.java.io.inputStream._
import pimpathon.util._
import pimpathon.any._


class InputStreamTest {
  @Test def attemptClose: Unit = {
    assertEquals(Success(()), createInputStream().attemptClose())
    assertEquals(Failure(boom), createInputStream(onClose = () => throw boom).attemptClose())
  }

  @Test def closeAfter: Unit = {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertEquals("result", is.closeAfter(_ => "result"))
    assertInputStreamClosed(true, is.closed)
  }

  @Test def closeIf: Unit = {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertInputStreamClosed(false, is.closeIf(false).closed)
    assertInputStreamClosed(true,  is.closeIf(true).closed)
  }

  @Test def closeUnless: Unit = {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertInputStreamClosed(false, is.closeUnless(true).closed)
    assertInputStreamClosed(true,  is.closeUnless(false).closed)
  }

  @Test def drain: Unit = {
    for {
      expectedCloseIn  <- List(false, true)
      expectedCloseOut <- List(false, true)
      input            <- List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input), createOutputStream())

      is.drain(os, expectedCloseIn, expectedCloseOut)

      assertEquals(input, os.toString)
      assertInputStreamClosed(expectedCloseIn, is.closed)
      assertOutputStreamClosed(expectedCloseOut, os.closed)
    }

    ignoreExceptions {
      val (is, os) = (createInputStream(), createOutputStream())

      is.drain(os, closeOut = false)
      is.drain(os, closeIn = false)
      is.drain(os, closeOut = false, closeIn = false)
      is.drain(os, closeIn = false, closeOut = false)
    }
  }

  @Test def >> : Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())

    is >> os

    assertEquals("content", os.toString)
    assertInputStreamClosed(false, is.closed)
    assertOutputStreamClosed(false, os.closed)
  }

  @Test def buffered: Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())
    (is.buffered: BufferedInputStream).drain(os)

    assertEquals("content", os.toString)
  }

  @Test def readUpToN: Unit =  {
    def read(text : String, n : Int) = {
      val (is, os) = (createInputStream(text), createOutputStream())
      is.readUpToN(os, n)
      os.tap(_.close()).toString
    }

    assertEquals("cont", read("contents", 4))
    assertEquals("contents", read("contents", 8))
    assertEquals("contents", read("contents", 9))
    assertEquals("", read("contents", 0))
    intercept[IllegalArgumentException](read("contents", -1))
  }

  @Test def readN: Unit =  {
    def read(text : String, n : Int) = {
      val (is, os) = (createInputStream(text), createOutputStream())
      is.readN(os, n)
      os.tap(_.close()).toString
    }

    assertEquals("cont", read("contents", 4))
    assertEquals("contents", read("contents", 8))
    assertEquals("", read("contents", 0))
    intercept[IllegalArgumentException](read("contents", -1))
    intercept[IOException](read("contents", 9))
  }
}
