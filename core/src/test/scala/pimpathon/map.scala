package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.util.control._

import org.junit.Assert._
import pimpathon.map._


class MapTest {
  @Test def getOrThrow {
    assertEquals("missing", util.intercept[IllegalArgumentException] {
      Map(1 -> 2).getOrThrow(0, "missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      Map(1 -> 2).getOrThrow(0, new RuntimeException("missing"))
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

  @Test def multiMapCBF {
    val cbf = MultiMap.build[List, Int, String]
    val builder = cbf.apply()

    builder += ((1, "foo"))
    builder += ((1, "bar"))
    assertEquals(Map(1 -> List("foo", "bar")), builder.result())

    builder.clear()
    assertEquals(Map(), builder.result())
  }

  @Test def ignoreFromCBF {
    val ucbf = new UnitCanBuildFrom[List[Int], Int]

    assertEquals(UnitBuilder[Int]("apply()"), ucbf.apply())
    assertEquals(UnitBuilder[Int]("apply(List(1, 2, 3))"), ucbf.apply(List(1, 2, 3)))

    val ucbfi = new UnitCanBuildFrom[List[Int], Int] with IgnoreFromCBF[List[Int], Int, Unit]

    assertEquals(UnitBuilder[Int]("apply()"), ucbfi.apply())
    assertEquals(UnitBuilder[Int]("apply()"), ucbfi.apply(List(1, 2, 3)))
  }

  class UnitCanBuildFrom[From, Elem] extends CanBuildFrom[From, Elem, Unit] {
    def apply(): M.Builder[Elem, Unit]           = UnitBuilder[Elem]("apply()")
    def apply(from: From): M.Builder[Elem, Unit] = UnitBuilder[Elem](s"apply($from)")
  }

  case class UnitBuilder[E](from: String) extends M.Builder[E, Unit] {
    def +=(elem: E): this.type = this
    def clear(): Unit = {}
    def result(): Unit = ()
    override def toString = s"UnitBuilder($from)"
  }
}
