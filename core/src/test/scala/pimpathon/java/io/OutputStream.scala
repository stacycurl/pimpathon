package pimpathon.java.io

import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import org.junit.Test

import org.junit.Assert._
import pimpathon.java.io.outputStream._


class OutputStreamTest {
  @Test def write {
    def assertWrite(input: String): Unit = {
      val is: InputStream = new ByteArrayInputStream(input.getBytes)
      val os: OutputStream = new ByteArrayOutputStream()

      assertEquals(input, os.write(is).toString)
    }

    List("Input", "Repeat" * 100).foreach(assertWrite)
  }
}
