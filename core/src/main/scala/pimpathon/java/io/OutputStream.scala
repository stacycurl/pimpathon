package pimpathon.java.io

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

import pimpathon.java.io.inputStream._


object outputStream {
  implicit class OutputStreamOps(val os: OutputStream) extends AnyVal {
    def write(is: InputStream): OutputStream = { is.read(os); os }
  }
}
