package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test

import org.junit.Assert._
import pimpathon.java.io.inputStream._
import pimpathon.util._


class InputStreamTest {
  @Test def attemptClose {
    assertEquals(Right(()), createInputStream().attemptClose())
    assertEquals(Left(boom), createInputStream(onClose = () => throw boom).attemptClose())
  }

  @Test def closeAfter {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertEquals("result", is.closeAfter(_ => "result"))
    assertInputStreamClosed(true, is.closed)
  }

  @Test def closeIf {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertInputStreamClosed(false, is.closeIf(false).closed)
    assertInputStreamClosed(true,  is.closeIf(true).closed)
  }

  @Test def closeUnless {
    val is = createInputStream()

    assertInputStreamClosed(false, is.closed)
    assertInputStreamClosed(false, is.closeUnless(true).closed)
    assertInputStreamClosed(true,  is.closeUnless(false).closed)
  }

  @Test def drain {
    for {
      expectedCloseIn  <- List(false, true)
      expectedCloseOut <- List(false, true)
      input            <- List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input.getBytes), createOutputStream())

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

  @Test def >> {
    for {
      expectedCloseIn  <- List(false, true)
      expectedCloseOut <- List(false, true)
      input            <- List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input.getBytes), createOutputStream())
      val inputStream = InputStreamUtils(expectedCloseIn, expectedCloseOut)
      import inputStream._

      is >> os

      assertEquals(input, os.toString)
      assertInputStreamClosed(expectedCloseIn, is.closed)
      assertOutputStreamClosed(expectedCloseOut, os.closed)
    }
  }
}
