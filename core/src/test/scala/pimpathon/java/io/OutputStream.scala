package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test

import org.junit.Assert._
import pimpathon.java.io.outputStream._
import pimpathon.util._


class OutputStreamTest {
  @Test def closeIf {
    val os = createOutputStream()
    assertInputStreamClosed(false, os.closed)

    os.closeIf(false)
    assertInputStreamClosed(false, os.closed)

    os.closeIf(true)
    assertInputStreamClosed(true, os.closed)
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
