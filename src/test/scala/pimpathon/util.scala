package pimpathon

import scala.language.implicitConversions

import _root_.java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import _root_.java.util.concurrent.atomic.AtomicBoolean

import scala.{PartialFunction ⇒ ~>}
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable ⇒ M}
import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Try}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.list._
import pimpathon.tuple._
import pimpathon.pimpTry._


object util {
  implicit class AnyTestPimp[A](val self: A) extends AnyVal {
    def ===(expected: A): Unit = assertEquals(expected, self)
  }

  def ignore(f: ⇒ Unit): Unit = {}
  def ignoreExceptions(f: ⇒ Unit): Unit = Try(f)

  def assertThrows[T <: Throwable: ClassTag](expectedMessage: String)(f: ⇒ Any): Unit =
    assertThrows(f) === expectedMessage

  def assertThrows[T <: Throwable: ClassTag](f: ⇒ Any): String =
    getMessage[T](f).getOrElse(sys.error("Expected exception: " + classTag.className[T]))

  def assertEqualsSet[A](expected: Set[A], actual: Set[A]): Unit = (expected -- actual, actual -- expected).calcC(
    missing ⇒ extra ⇒ assertTrue(s"Extra: $extra, Missing: $missing", extra.isEmpty && missing.isEmpty)
  )

  def intercept[T <: Throwable: ClassTag](f: ⇒ Unit): T = 
    getThrowable[T](f).getOrElse(sys.error("Expected exception: " + classTag.className[T]))

  case class calling[A, B](f: A ⇒ B) {
    def partitions(as: A*): partitions = partitions(as.toList)
    case class partitions(as: List[A]) {
      def into(expectations: (Expectation[A, B])*): Unit = {
        val (expectedFailbackFns, abs) = expectations.map(_.value).toList.partitionEithers[List]

        expectedFailbackFns.onlyOption match {
          case None ⇒ pairs(as.rpair(f)) === pairs(abs)
          case Some(expectedFallbackFn) ⇒ {
            as.partition(a ⇒ abs.exists(_._1 == a)).calcC(positive ⇒ remainder ⇒ {
              val fallbacks = remainder.rpair(expectedFallbackFn)

              pairs((positive ::: remainder).rpair(f)) === pairs(abs ::: fallbacks)
            })
          }
        }
      }
    }
  }

  case class on[A](as: A*) {
    case class calling[B](fs: (A ⇒ B)*) {
      def produces(bs: B*): Unit    = (for {f ← fs; a ← as} yield f(a)).toList === bs.toList
      def throws(es: String*): Unit = (for {f ← fs; a ← as; m ← Try(f(a)).getMessage} yield m).toList === es.toList
    }
  }

  implicit class onFnPimps[A, B](private val self: on[A ⇒ B]) {
    def maps[C](abs: (A, B)*): Unit = applying(abs.map(_._1): _*).produces(abs.map(_._2): _*)

    def applying(as: A*): on[Unit]#calling[B] = on(()).calling[B]((for {
      ab ← self.as
      a  ← as
    } yield (_: Unit) ⇒ ab.apply(a)): _*)
  }

  object Expectation {
    implicit def tupleAsExpectation[A, B](ab: (A, B)):   Expectation[A, B]       = Expectation(Right(ab))
    implicit def otherAsExpectation[A, B](ob: (others, B)): Expectation[A, B] = Expectation(Left(_ ⇒ ob._2))

    implicit def othersUnchangedAsExpectation[A](ou: (others, unchanged)): Expectation[A, A] =
      Expectation[A, A](Left((a: A) ⇒ a))
  }

  case class Expectation[A, +B](value: Either[A ⇒ B, (A, B)])

  private def pairs[A, B](abs: List[(A, B)]): String = abs.mapC(a ⇒ b ⇒ s"$a → $b").mkString(", ")

  def createInputStream(content: String = ""): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content.getBytes) with Closeable

  def createInputStream(content: Array[Byte]): ByteArrayInputStream with Closeable =
    new ByteArrayInputStream(content) with Closeable

  def createOutputStream(): ByteArrayOutputStream with Closeable =
    new ByteArrayOutputStream with Closeable

  private def getMessage[T <: Throwable: ClassTag](f: ⇒ Unit): Option[String] = getThrowable[T](f).map(_.getMessage)

  private def getThrowable[T <: Throwable: ClassTag](f: ⇒ Unit): Option[T] = try { f; None } catch {
    case t: Throwable ⇒ Some(t.castTo[T].getOrElse(sys.error(s"Invalid exception, expected ${classTag.className[T]}, got: $t")))
  }

  def goBoom: Nothing = throw boom
  val boom = new Throwable("Boom !")
  def exception[A](message: A): Exception = new Exception(message.toString)

  def currentTime(): Long = dynamicTime.value
  def withTime[A](millis: Long)(f: ⇒ A): A = dynamicTime.withValue(millis)(f)

  def partial[A, B](entries: (A, B)*): A ~> B = entries.toMap

  def ints(is: Int*): ListBuffer[Int] = new M.ListBuffer[Int] ++= is
  def strings(ss: String*): ListBuffer[String] = new M.ListBuffer[String] ++= ss

  def nil[A]: List[A] = Nil
  sealed trait others;    case object others    extends others
  sealed trait unchanged; case object unchanged extends unchanged

  trait Closeable extends _root_.java.io.Closeable {
    abstract override def close(): Unit = { closed.set(true); super.close() }

    def assertOpen: Unit  = assertFalse(s"expected $kind to be open",   closed.get())
    def assertClosed: Unit = assertTrue(s"expected $kind to be closed", closed.get())

    private val closed = new AtomicBoolean(false)
    private def kind = if (this.isInstanceOf[ByteArrayInputStream]) "InputStream" else "OutputStream"
  }

  private val dynamicTime = new DynamicVariable[Long](0)
}