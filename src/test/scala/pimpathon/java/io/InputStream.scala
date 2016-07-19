package pimpathon.java.io

import java.nio.charset.Charset

import scala.language.reflectiveCalls

import java.io._
import java.util.zip.GZIPOutputStream
import org.junit.Test
import scala.util.{Failure, Success}

import pimpathon.util._
import pimpathon.any._


class InputStreamTest {
  @Test def attemptClose(): Unit = {
    createInputStream().attemptClose() === Success(())
    new ByteArrayInputStream(Array()) { override def close() = goBoom }.attemptClose() === Failure(boom)
  }

  @Test def closeAfter(): Unit = {
    val is = createInputStream()

    is.closeAfter(_ ⇒ "result") === "result"
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

      os.toString === input
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

    os.toString === "content"
    is.assertOpen
    os.assertOpen
  }

  @Test def buffered(): Unit = {
    val (is, os) = (createInputStream("content"), createOutputStream())
    (is.buffered: BufferedInputStream).drain(os)

    os.toString === "content"
  }

  @Test def gunzip(): Unit = {
    import pimpathon.java.io.outputStream._

    val os     = createOutputStream().tap(os ⇒ new GZIPOutputStream(os).closeAfter(_.write("content".getBytes)))
    val result = createOutputStream().tap(rs ⇒ createInputStream(os.toByteArray).gunzip.drain(rs))

    result.toString === "content"
  }

  @Test def readUpToN(): Unit = {
    def read(text: String, n: Int, bufferSize: Int = inputStream.bufferSize): String = {
      val withBufferSize = new InputStreamUtils(bufferSize = bufferSize); import withBufferSize._
      val (is, os) = (createInputStream(text), createOutputStream())

      os.tap(is.readUpToN(_, n), _.close()).toString
    }

    read("contents", 0)    === ""
    read("contents", 4)    === "cont"
    read("contents", 8)    === "contents"
    read("contents", 9)    === "contents"
    read("contents", 7, 2) === "content"
    read("content",  8, 2) === "content"

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }
  }

  @Test def readN(): Unit = {
    def read(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(is.readN(_, n), _.close()).toString
    }

    read("contents", 0) === ""
    read("contents", 4) === "cont"
    read("contents", 8) === "contents"

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      read("contents", -1)
    }

    assertThrows[IOException]("Failed to read 9 bytes, only 8 were available")(read("contents", 9))
  }

  @Test def toByteArray(): Unit = new String(createInputStream("contents").toByteArray) === "contents"

  @Test def asString(): Unit = {
    new ByteArrayInputStream("contents".getBytes).asString === "contents"
    val UTF8 = Charset.forName("UTF-8")
    new ByteArrayInputStream("contents".getBytes(UTF8)).asString(UTF8) === "contents"
  }
}