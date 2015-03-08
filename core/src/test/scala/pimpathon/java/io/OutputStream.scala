package pimpathon.java.io

import java.io._
import java.util.zip.GZIPInputStream
import org.junit.Test

import org.junit.Assert._
import pimpathon.util._
import pimpathon.any._


class OutputStreamTest {
  @Test def attemptClose(): Unit = {
    assertEquals(Right(()), createOutputStream().attemptClose())
    assertEquals(Left(boom), new ByteArrayOutputStream() { override def close() = goBoom }.attemptClose())
  }

  @Test def closeAfter(): Unit = {
    val os = createOutputStream()

    assertEquals("result", os.closeAfter(_ ⇒ "result"))
    os.assertClosed
  }

  @Test def closeIf(): Unit = {
    createOutputStream().closeIf(condition = false).assertOpen
    createOutputStream().closeIf(condition = true).assertClosed
  }

  @Test def closeUnless(): Unit = {
    createOutputStream().closeUnless(condition = true).assertOpen
    createOutputStream().closeUnless(condition = false).assertClosed
  }

  @Test def drain(): Unit = {
    for {
      expectedCloseIn  ← List(false, true)
      expectedCloseOut ← List(false, true)
      input            ← List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input), createOutputStream())

      os.drain(is, expectedCloseOut, expectedCloseIn)

      assertEquals(input, os.toString)
      if (expectedCloseOut) os.assertClosed else os.assertOpen
      if (expectedCloseIn)  is.assertClosed else is.assertOpen
    }

    ignoreExceptions {
      val (is, os) = (createInputStream(), createOutputStream())

      os.drain(is, closeOut = false)
      os.drain(is, closeIn = false)
      os.drain(is, closeOut = false, closeIn = false)
      os.drain(is, closeIn = false, closeOut = false)
    }
  }

  @Test def << (): Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())

    os << is

    assertEquals("content", os.toString)
    os.assertOpen
    is.assertOpen
  }

  @Test def buffered(): Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())

    assertEquals("content", os.tap(o ⇒ (o.buffered: BufferedOutputStream).drain(is)).toString)
  }

  @Test def gzip(): Unit = {
    val os     = createOutputStream().tap(_.gzip.closeAfter(_.write("content".getBytes)))
    val result = createOutputStream().tap(rs ⇒ new GZIPInputStream(createInputStream(os.toByteArray)).drain(rs))

    assertEquals("content", result.toString)
  }

  @Test def writeUpToN(): Unit = {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeUpToN(is, n), _.close()).toString
    }

    assertEquals("cont", write("contents", 4))
    assertEquals("contents", write("contents", 8))
    assertEquals("contents", write("contents", 9))
    assertEquals("", write("contents", 0))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }
  }

  @Test def writeN(): Unit = {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeN(is, n), _.close()).toString
    }

    assertEquals("cont", write("contents", 4))
    assertEquals("contents", write("contents", 8))
    assertEquals("", write("contents", 0))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }

    assertThrows[IOException]("Failed to write 9 only 8 were available")(write("contents", 9))
  }
}