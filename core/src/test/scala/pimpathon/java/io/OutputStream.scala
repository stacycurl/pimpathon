package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test
import scala.util.{Failure, Success}

import org.junit.Assert._
import pimpathon.java.io.outputStream._
import pimpathon.util._


class OutputStreamTest {
  @Test def attemptClose {
    assertEquals(Success(()), createOutputStream().attemptClose())
    assertEquals(Failure(boom), createOutputStream(onClose = () => throw boom).attemptClose())
  }

  @Test def closeIf {
    val os = createOutputStream()

    assertInputStreamClosed(false, os.closed)
    assertInputStreamClosed(false, os.closeIf(false).closed)
    assertInputStreamClosed(true,  os.closeIf(true).closed)
  }

  @Test def closeUnless {
    val os = createOutputStream()

    assertInputStreamClosed(false, os.closed)
    assertInputStreamClosed(false, os.closeUnless(true).closed)
    assertInputStreamClosed(true,  os.closeUnless(false).closed)
  }

  @Test def write {
    for {
      expectedCloseIn  <- List(false, true)
      expectedCloseOut <- List(false, true)
      input            <- List("Input", "Repeat" * 100)
    } {
      val (is, os) = (createInputStream(input.getBytes), createOutputStream())

      os.write(is, expectedCloseOut, expectedCloseIn)

      assertEquals(input, os.toString)
      assertOutputStreamClosed(expectedCloseOut, os.closed)
      assertInputStreamClosed(expectedCloseIn, is.closed)
    }
  }
}
