package pimpathon.java.io

import java.nio.charset.Charset

import scala.language.reflectiveCalls

import java.io._
import java.util.zip.GZIPOutputStream
import org.junit.Test
import scala.util.{Failure, Success}

import org.junit.Assert._
import pimpathon.util._
import pimpathon.any._


class InputStreamTest {
  @Test def attemptClose(): Unit = {
    assertEquals(Success(()), createInputStream().attemptClose())
    assertEquals(Failure(boom), new ByteArrayInputStream(Array()) { override def close() = goBoom }.attemptClose())
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
    for { closeIn ← List(false, true); closeOut ← List(false, true); input ← List("Input", "Repeat" * 100) } {
      val (is, os) = (createInputStream(input), createOutputStream())

      is.drain(os, closeIn, closeOut)

      assertEquals(input, os.toString)
      if (closeIn)  is.assertClosed else is.assertOpen
      if (closeOut) os.assertClosed else os.assertOpen
    }

    ignoreExceptions { // Merely verifying (via compilation) that named parameters works, bit redundant.
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

    assertEquals("",         read("contents", 0))
    assertEquals("cont",     read("contents", 4))
    assertEquals("contents", read("contents", 8))
    assertEquals("contents", read("contents", 9))
    assertEquals("content",  read("contents", 7, 2))
    assertEquals("content",  read("content",  8, 2))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }
  }

  @Test def readN(): Unit = {
    def read(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(is.readN(_, n), _.close()).toString
    }

    assertEquals("",         read("contents", 0))
    assertEquals("cont",     read("contents", 4))
    assertEquals("contents", read("contents", 8))

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }

    assertThrows[IOException]("Failed to read 9 bytes, only 8 were available")(read("contents", 9))
  }

  @Test def toByteArray(): Unit = {
    assertEquals("contents", new String(createInputStream("contents").toByteArray))
  }

  @Test def asString(): Unit = {
    assertEquals("contents", new ByteArrayInputStream("contents".getBytes).asString)
    val UTF8 = Charset.forName("UTF-8")
    assertEquals("contents", new ByteArrayInputStream("contents".getBytes(UTF8)).asString(UTF8))
  }
}