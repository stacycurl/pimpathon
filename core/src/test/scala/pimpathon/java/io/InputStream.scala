package pimpathon.java.io

import java.io._
import java.util.zip.GZIPOutputStream
import org.junit.Test

import org.junit.Assert._
import pimpathon.util._
import pimpathon.any._


class InputStreamTest {
  @Test def attemptClose(): Unit = {
    assertEquals(Right(()), createInputStream().attemptClose())
    assertEquals(Left(boom), new ByteArrayInputStream(Array()) { override def close() = goBoom }.attemptClose())
  }

  @Test def closeAfter(): Unit = {
    val is = createInputStream()

    assertEquals("result", is.closeAfter(_ ⇒ "result"))
    is.assertClosed
  }

  @Test def closeIf(): Unit = {
    createInputStream().closeIf(condition = false).assertOpen
    createInputStream().closeIf(condition = true).assertClosed
  }

  @Test def closeUnless(): Unit = {
    createInputStream().closeUnless(condition = true).assertOpen
    createInputStream().closeUnless(condition = false).assertClosed
  }

  @Test def drain(): Unit = {
    for {
      expectedCloseIn  ← List(false, true)
      expectedCloseOut ← List(false, true)
      input            ← List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input), createOutputStream())

      is.drain(os, expectedCloseIn, expectedCloseOut)

      assertEquals(input, os.toString)
      if (expectedCloseIn)  is.assertClosed else is.assertOpen
      if (expectedCloseOut) os.assertClosed else os.assertOpen
    }

    ignoreExceptions {
      val (is, os) = (createInputStream(), createOutputStream())

      is.drain(os, closeOut = false)
      is.drain(os, closeIn = false)
      is.drain(os, closeOut = false, closeIn = false)
      is.drain(os, closeIn = false, closeOut = false)
    }
  }

  @Test def >> (): Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())

    is >> os

    assertEquals("content", os.toString)
    is.assertOpen
    os.assertOpen
  }

  @Test def buffered(): Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())
    (is.buffered: BufferedInputStream).drain(os)

    assertEquals("content", os.toString)
  }

  @Test def gunzip(): Unit = {
    import pimpathon.java.io.outputStream._

    val os     = createOutputStream().tap(os ⇒ new GZIPOutputStream(os).closeAfter(_.write("content".getBytes)))
    val result = createOutputStream().tap(rs ⇒ createInputStream(os.toByteArray).gunzip.drain(rs))

    assertEquals("content", result.toString)
  }

  @Test def readUpToN(): Unit = {
    def read(text: String, n: Int, bufferSize: Int = inputStream.bufferSize): String = {
      val withBufferSize = new InputStreamUtils(bufferSize = bufferSize); import withBufferSize._
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(is.readUpToN(_, n), _.close()).toString
    }

    assertEquals("cont", read("contents", 4))
    assertEquals("contents", read("contents", 8))
    assertEquals("content", read("contents", 7, 2))
    assertEquals("content", read("content", 8, 2))
    assertEquals("contents", read("contents", 9))
    assertEquals("", read("contents", 0))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }
  }

  @Test def readN(): Unit = {
    def read(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(is.readN(_, n), _.close()).toString
    }

    assertEquals("cont", read("contents", 4))
    assertEquals("contents", read("contents", 8))
    assertEquals("", read("contents", 0))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }

    assertThrows[IOException]("Failed to read 9 bytes, only 8 were available")(read("contents", 9))
  }
}