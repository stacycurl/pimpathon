package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
import pimpathon.java.io.inputStream._
import pimpathon.util._


class InputStreamTest {
  @Test def attemptClose {
    assertEquals(Right(()), createInputStream().attemptClose())

    new Throwable("boom").tap(boom => {
      assertEquals(Left(boom), createInputStream(onClose = () => throw boom).attemptClose())
    })
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

  @Test def read {
    for {
      expectedCloseIn  <- List(false, true)
      expectedCloseOut <- List(false, true)
      input            <- List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input.getBytes), createOutputStream())

      is.read(os, expectedCloseIn, expectedCloseOut)

      assertEquals(input, os.toString)
      assertInputStreamClosed(expectedCloseIn, is.closed)
      assertOutputStreamClosed(expectedCloseOut, os.closed)
    }
  }
}
