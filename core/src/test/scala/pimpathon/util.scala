package pimpathon

import _root_.java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import _root_.java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable ⇒ M}
import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Try}

import org.junit.Assert._


object util {
  def assertException[E <: Throwable](expectedMessage: String)(f: ⇒ Unit)
    (implicit expected: ClassTag[E]): Unit = assertEquals(expectedMessage, intercept[E](f).getMessage)

  def assertEqualsSet[A](expected: Set[A], actual: Set[A]): Unit = {
    val (missing, extra) = (expected -- actual, actual -- expected)

    assertTrue(s"Extra: $extra, Missing: $missing", extra.isEmpty && missing.isEmpty)
  }

  def createInputStream(content: String = ""): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content.getBytes) with Closeable

  def createInputStream(content: Array[Byte]): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content) with Closeable

  def createOutputStream(): ByteArrayOutputStream with Closeable =
    new ByteArrayOutputStream with Closeable

  def ignoreExceptions(f: ⇒ Unit): Unit = Try(f)

  def intercept[E <: Throwable](f: ⇒ Any)(implicit expected: ClassTag[E]): E = {
    val clazz = expected.runtimeClass

    val caught = try { f; None } catch {
      case u: Throwable ⇒ if (clazz.isAssignableFrom(u.getClass)) Some(u) else {
        sys.error(s"Invalid exception, expected ${clazz.getName}, got: " + u)
      }
    }

    caught match {
      case None ⇒ sys.error(s"Expected exception: ${clazz.getName}")
      case Some(e) ⇒ e.asInstanceOf[E]
    }
  }

  def goBoom: Nothing = throw boom
  val boom = new Throwable("Boom !")

  def currentTime(): Long = dynamicTime.value
  def withTime[A](millis: Long)(f: ⇒ A): A = dynamicTime.withValue(millis)(f)

  def partial[A, B](entries: (A, B)*): PartialFunction[A, B] = entries.toMap

  def ints(is: Int*): ListBuffer[Int] = new M.ListBuffer[Int] ++= is
  def strings(ss: String*): ListBuffer[String] = new M.ListBuffer[String] ++= ss

  def nil[A]: List[A] = Nil

  trait Closeable extends _root_.java.io.Closeable {
    abstract override def close(): Unit = { closed.set(true); super.close() }

    def assertOpen: Unit = assertEquals(s"expected $kind to be open", false, closed.get())
    def assertClosed: Unit = assertEquals(s"expected $kind to be closed", true, closed.get())

    private val closed = new AtomicBoolean(false)
    private def kind = if (this.isInstanceOf[ByteArrayInputStream]) "InputStream" else "OutputStream"
  }

  private val dynamicTime = new DynamicVariable[Long](0)
}