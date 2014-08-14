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
    def assertWrite(input: String): Unit = {
      val is: InputStream = new ByteArrayInputStream(input.getBytes)
      val os: OutputStream = new ByteArrayOutputStream()

      assertEquals(input, os.write(is).toString)
    }

    List("Input", "Repeat" * 100).foreach(assertWrite)
  }
}
