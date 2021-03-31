package pimpathon.java.io

import java.io._
import java.util.zip.GZIPInputStream
import pimpathon.PSpec
import pimpathon.any._

import scala.util.{Failure, Success}


class OutputStreamSpec extends PSpec {
  "attemptClose" in {
    createOutputStream().attemptClose() ≡ Success(())
    new ByteArrayOutputStream() { override def close(): Unit = goBoom }.attemptClose() ≡ Failure(boom)
  }

  "closeAfter" in {
    val os = createOutputStream()

    os.closeAfter(_ ⇒ "result") ≡ "result"
    os.assertClosed
  }

  "closeIf" in {
    createOutputStream().closeIf(condition = false).assertOpen
    createOutputStream().closeIf(condition = true).assertClosed
  }

  "closeUnless" in {
    createOutputStream().closeUnless(condition = true).assertOpen
    createOutputStream().closeUnless(condition = false).assertClosed
  }

  "drain" in {
    for { closeIn ← List(false, true); closeOut ← List(false, true); input ← List("Input", "Repeat" * 100) } {
      val (is, os) = (createInputStream(input), createOutputStream())

      os.drain(is, closeOut, closeIn)

      os.toString ≡ input
      if (closeOut) os.assertClosed else os.assertOpen
      if (closeIn)  is.assertClosed else is.assertOpen
    }

    ignoreExceptions { // Merely verifying (via compilation) that named parameters works, bit redundant.
      val (is, os) = (createInputStream(), createOutputStream())

      os.drain(is, closeOut = false)
      os.drain(is, closeIn = false)
      os.drain(is, closeOut = false, closeIn = false)
      os.drain(is, closeIn = false, closeOut = false)
    }
  }

  "<<" in {
    val (is, os) = (createInputStream("content"), createOutputStream())

    os << is

    os.toString ≡ "content"
    os.assertOpen
    is.assertOpen
  }

  "buffered" in {
    val (is, os) = (createInputStream("content"), createOutputStream())

    os.tap(o ⇒ (o.buffered: BufferedOutputStream).drain(is)).toString ≡ "content"
  }

  "gzip" in {
    val os     = createOutputStream().tap(_.gzip.closeAfter(_.write("content".getBytes)))
    val result = createOutputStream().tap(rs ⇒ new GZIPInputStream(createInputStream(os.toByteArray)).drain(rs))

    result.toString ≡ "content"
  }

  "writeUpToN" in {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeUpToN(is, n), _.close()).toString
    }

    write("contents", 4) ≡ "cont"
    write("contents", 8) ≡ "contents"
    write("contents", 9) ≡ "contents"
    write("contents", 0) ≡ ""

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }
  }

  "writeN" in {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeN(is, n), _.close()).toString
    }

    write("contents", 4) ≡ "cont"
    write("contents", 8) ≡ "contents"
    write("contents", 0) ≡ ""

    assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }

    assertThrows[IOException]("Failed to write 9 only 8 were available")(write("contents", 9))
  }
}