package stacycurl.scala.pimpathon

import org.junit.Test
import scala.collection.{mutable => M}
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

  @Test def uncons {
    assertEquals("empty", Map.empty[Int, Int].uncons("empty", _ => "nonEmpty"))
    assertEquals("nonEmpty", Map(1 -> 2).uncons("empty", _ => "nonEmpty"))
  }

  @Test def emptyTo {
    assertEquals(Map(1 -> 2), Map.empty[Int, Int].emptyTo(Map(1 -> 2)))
    assertEquals(Map(3 -> 4), Map(3 -> 4).emptyTo(Map(1 -> 2)))
  }

  @Test def valueForMaxKey {
    assertEquals(None, Map.empty[Int, String].valueForMaxKey)
    assertEquals(Some("max"), Map(1 -> "min", 2 -> "max").valueForMaxKey)
  }

  @Test def valueForMinKey {
    assertEquals(None, Map.empty[Int, String].valueForMinKey)
    assertEquals(Some("min"), Map(1 -> "min", 2 -> "max").valueForMinKey)
  }

  @Test def keyForMaxValue {
    assertEquals(None, Map.empty[Int, String].keyForMaxValue)
    assertEquals(Some(2), Map(1 -> "abc", 2 -> "def").keyForMaxValue)
  }

  @Test def keyForMinValue {
    assertEquals(None, Map.empty[Int, String].keyForMinValue)
    assertEquals(Some(2), Map(2 -> "abc", 2 -> "def").keyForMinValue)
  }

  @Test def mapValuesEagerly {
    val originalValuesSeen = new M.ListBuffer[Int]
    def update(v: Int) = { originalValuesSeen += v; v * 10 }

    val result = Map(1 -> 1, 2 -> 2).mapValuesEagerly(update)
    assertEquals("Should have iterated over the original map already", List(1, 2), originalValuesSeen.toList)
    assertEquals(Map(1 -> 10, 2 -> 20), result)
    assertEquals(Map(1 -> 10, 2 -> 20), result)
    assertEquals("Shouldn't have iterated over the original map twice", List(1, 2), originalValuesSeen.toList)
  }

  private def intercept[E <: AnyRef](f: => Any)(implicit expected: ClassTag[E]): E = {
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
