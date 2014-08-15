package pimpathon

import scala.reflect.ClassTag
import scala.util.Try
import _root_.java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}

import org.junit.Assert._


object util {
  def assertInputStreamClosed(expected: Boolean, closed: Boolean): Unit =
    assertEquals("expected InputStream to %s closed".format(if (expected) "be" else "not be"), expected, closed)

  def assertOutputStreamClosed(expected: Boolean, closed: Boolean): Unit =
    assertEquals("expected OutputStream to %s closed".format(if (expected) "be" else "not be"), expected, closed)

  def createInputStream(bytes: Array[Byte] = Array(), onClose: () => Unit = () => {}) = new ByteArrayInputStream(bytes) {
    var closed = false
    override def close() = { closed = true; super.close(); onClose() }
  }

  def createOutputStream(onClose: () => Unit = () => {}) = new ByteArrayOutputStream() {
    var closed = false
    override def close() = { closed = true; super.close(); onClose() }
  }

  def ignoreExceptions(f: => Unit): Unit = Try(f)

  def intercept[E <: AnyRef](f: => Any)(implicit expected: ClassTag[E]): E = {
    val clazz = expected.runtimeClass

    val caught = try { f; None } catch {
      case u: Throwable => if (clazz.isAssignableFrom(u.getClass)) Some(u) else {
        sys.error(s"Invalid exception, expected ${clazz.getName}, got: " + u)
      }
    }

    caught match {
      case None => sys.error(s"Expected exception: ${clazz.getName}")
      case Some(e) => e.asInstanceOf[E]
    }
  }

  val boom = new Throwable("Boom !")
}
