package stacycurl.scala.pimpathon

import org.junit.Test
import scala.reflect.ClassTag
import scala.util.control._

import org.junit.Assert._
import stacycurl.scala.pimpathon.map._


class MapTest {
  @Test def getOrThrow {
    assertEquals("missing", intercept[RuntimeException] {
      Map(1 -> 2).getOrThrow(0, "missing")
    }.getMessage)
  }

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
}
