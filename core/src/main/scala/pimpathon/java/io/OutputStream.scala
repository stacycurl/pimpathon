package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.java.io.inputStream._


object outputStream {
  implicit def outputStreamOps(os: OutputStream): OutputStreamOps = new OutputStreamOps(os)

  class OutputStreamOps(os: OutputStream) {
    def write(is: InputStream): OutputStream = { is.read(os); os }
  }
}
