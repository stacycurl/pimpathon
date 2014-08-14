package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test

import org.junit.Assert._
import pimpathon.java.io.inputStream._
import pimpathon.util._


class InputStreamTest {
  @Test def closeIf {
    val is = createInputStream()
    assertInputStreamClosed(false, is.closed)

    is.closeIf(false)
    assertInputStreamClosed(false, is.closed)

    is.closeIf(true)
    assertInputStreamClosed(true, is.closed)
  }

  @Test def read {
    def assertRead(input: String): Unit = {
      val is: InputStream = new ByteArrayInputStream(input.getBytes)
      val os: OutputStream = new ByteArrayOutputStream()

      is.read(os)

      assertEquals(input, os.toString)
    }

    List("Input", "Repeat" * 100).foreach(assertRead)
  }
}
