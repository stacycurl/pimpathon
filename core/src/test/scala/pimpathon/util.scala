package pimpathon

import _root_.java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import _root_.java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable ⇒ M}
import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Try}

import org.junit.Assert._
import pimpathon.tuple._
import pimpathon.function._
import pimpathon.pimpTry._


object util {
  def assertThrows[E <: Throwable: ClassTag](expectedMessage: String)(f: ⇒ Unit): Unit =
    assertEquals(expectedMessage, getMessage(f).getOrElse(sys.error(s"Expected exception: ${classTag.className[E]}")))

  def assertEqualsSet[A](expected: Set[A], actual: Set[A]): Unit = (expected -- actual, actual -- expected).calcC(
    missing ⇒ extra ⇒ assertTrue(s"Extra: $extra, Missing: $missing", extra.isEmpty && missing.isEmpty)
  )

  case class on[A](as: A*) { def calling[B](f: A ⇒ B): Calling[A, B] = Calling(f, as: _*) }

  case class Calling[A, B](f: A ⇒ B, as: A*) {
    def produces(bs: B*): Unit = assertEquals(bs.toList, as.map(f).toList)
    def throws(es: String*): Unit = assertEquals(es.toList, as.map(a ⇒ f.attempt(a).getMessage).toList.flatten)
  }

  def createInputStream(content: String = ""): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content.getBytes) with Closeable

  def createInputStream(content: Array[Byte]): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content) with Closeable

  def createOutputStream(): ByteArrayOutputStream with Closeable =
    new ByteArrayOutputStream with Closeable

  def ignoreExceptions(f: ⇒ Unit): Unit = Try(f)

  private def getMessage[E <: Throwable: ClassTag](f: ⇒ Unit): Option[String] = try { f; None } catch {
    case u: Throwable ⇒ if (classTag.klassOf[E].isAssignableFrom(u.getClass)) Some(u.getMessage) else sys.error(
      s"Invalid exception, expected ${classTag.className[E]}, got: $u"
    )
  }

  def goBoom: Nothing = throw boom
  val boom = new Throwable("Boom !")
  def exception[A](message: A): Exception = new Exception(message.toString)

  def currentTime(): Long = dynamicTime.value
  def withTime[A](millis: Long)(f: ⇒ A): A = dynamicTime.withValue(millis)(f)

  def partial[A, B](entries: (A, B)*): PartialFunction[A, B] = entries.toMap

  def ints(is: Int*): ListBuffer[Int] = new M.ListBuffer[Int] ++= is
  def strings(ss: String*): ListBuffer[String] = new M.ListBuffer[String] ++= ss

  def nil[A]: List[A] = Nil

  trait Closeable extends _root_.java.io.Closeable {
    abstract override def close(): Unit = { closed.set(true); super.close() }

    def assertOpen: Unit  = assertFalse(s"expected $kind to be open",   closed.get())
    def assertClosed: Unit = assertTrue(s"expected $kind to be closed", closed.get())

    private val closed = new AtomicBoolean(false)
    private def kind = if (this.isInstanceOf[ByteArrayInputStream]) "InputStream" else "OutputStream"
  }

  private val dynamicTime = new DynamicVariable[Long](0)
}