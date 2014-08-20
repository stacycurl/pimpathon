package pimpathon

import scala.reflect.ClassManifest
import _root_.java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}

import org.junit.Assert._


object util {
  def assertException[E <: Throwable](expectedMessage: String)(f: => Unit)
    (implicit expected: ClassManifest[E]): Unit = assertEquals(expectedMessage, intercept[E](f).getMessage)

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

  def ignoreExceptions(f: => Unit): Unit = try f catch { case t: Throwable => }

  def intercept[E <: Throwable](f: => Any)(implicit expected: ClassManifest[E]): E = {
    val clazz = expected.erasure

    val caught = try { f; None } catch {
      case u: Throwable => if (clazz.isAssignableFrom(u.getClass)) Some(u) else {
        sys.error("Invalid exception, expected %s, got: ".format(clazz.getName) + u)
      }
    }

    caught match {
      case None => sys.error("Expected exception: %s".format(clazz.getName))
      case Some(e) => e.asInstanceOf[E]
    }
  }

  val boom = new Throwable("Boom !")
}
