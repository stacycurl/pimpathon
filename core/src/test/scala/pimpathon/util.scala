package pimpathon

import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Try}
import _root_.java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}

import org.junit.Assert._


object util {
  def assertException[E <: Throwable](expectedMessage: String)(f: => Unit)
    (implicit expected: ClassTag[E]): Unit = assertEquals(expectedMessage, intercept[E](f).getMessage)

  def assertInputStreamClosed(expected: Boolean, closed: Boolean): Unit =
    assertEquals("expected InputStream to %s closed".format(if (expected) "be" else "not be"), expected, closed)

  def assertOutputStreamClosed(expected: Boolean, closed: Boolean): Unit =
    assertEquals("expected OutputStream to %s closed".format(if (expected) "be" else "not be"), expected, closed)

  def assertEqualsSet[A](expected: Set[A], actual: Set[A]): Unit = {
    val (missing, extra) = (expected -- actual, actual -- expected)

    assertTrue(s"Extra: $extra, Missing: $missing", extra.isEmpty && missing.isEmpty)
  }

  def createInputStream(content: String = "", onClose: () => Unit = () => {}) =
    new ByteArrayInputStream(content.getBytes) {
      var closed = false
      override def close() = { closed = true; super.close(); onClose() }
    }

  def createOutputStream(onClose: () => Unit = () => {}) = new ByteArrayOutputStream() {
    var closed: Boolean = false
    override def close(): Unit = { closed = true; super.close(); onClose() }
  }

  def ignoreExceptions(f: => Unit): Unit = Try(f)

  def intercept[E <: Throwable](f: => Any)(implicit expected: ClassTag[E]): E = {
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

  def goBoom: Nothing = throw boom
  val boom = new Throwable("Boom !")

  def currentTime(): Long = dynamicTime.value
  def withTime[A](millis: Long)(f: => A): A = dynamicTime.withValue(millis)(f)

  private val dynamicTime = new DynamicVariable[Long](0)
}
