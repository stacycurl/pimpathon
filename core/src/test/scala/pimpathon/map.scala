package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.util.control._

import org.junit.Assert._
import pimpathon.map._
import pimpathon.multiMap._


class MapTest {
  @Test def containsAny {
    assertFalse(empty.containsAny(None))
    assertFalse(empty.containsAny(Some(1)))
    assertFalse(nonEmpty.containsAny(None))
    assertFalse(nonEmpty.containsAny(Some(2)))
    assertTrue(nonEmpty.containsAny(Some(1)))

    assertFalse(empty.containsAny(Nil))
    assertFalse(empty.containsAny(List(1)))
    assertFalse(nonEmpty.containsAny(Nil))
    assertFalse(nonEmpty.containsAny(List(2)))
    assertTrue(nonEmpty.containsAny(List(1)))
    assertTrue(nonEmpty.containsAny(List(1, 2)))
  }

  @Test def containsAll {
    assertTrue(empty.containsAll(None))
    assertFalse(empty.containsAll(Some(1)))
    assertTrue(nonEmpty.containsAll(None))
    assertFalse(nonEmpty.containsAll(Some(2)))
    assertTrue(nonEmpty.containsAll(Some(1)))

    assertTrue(empty.containsAll(Nil))
    assertFalse(empty.containsAll(List(1)))
    assertTrue(nonEmpty.containsAll(Nil))
    assertFalse(nonEmpty.containsAll(List(2)))
    assertTrue(nonEmpty.containsAll(List(1)))
    assertFalse(nonEmpty.containsAll(List(1, 2)))
  }

  @Test def get {
    assertEquals(None,    Map.empty[Int, Int].get(Some(1)))
    assertEquals(None,    Map.empty[Int, Int].get(None))
    assertEquals(None,    Map(1 -> 2).get(None))
    assertEquals(None,    Map(1 -> 2).get(Some(2)))
    assertEquals(Some(2), Map(1 -> 2).get(Some(1)))
  }

  @Test def getOrThrow {
    assertEquals("missing", util.intercept[IllegalArgumentException] {
      nonEmpty.getOrThrow(0, "missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      nonEmpty.getOrThrow(0, new RuntimeException("missing"))
    }.getMessage)
  }

  @Test def uncons {
    assertEquals("empty", empty.uncons("empty", _ => "nonEmpty"))
    assertEquals("nonEmpty", nonEmpty.uncons("empty", _ => "nonEmpty"))
  }

  @Test def mapNonEmpty {
    assertEquals(None, empty.mapNonEmpty(_ => "nonEmpty"))
    assertEquals(Some("nonEmpty"), nonEmpty.mapNonEmpty(_ => "nonEmpty"))
  }

  @Test def emptyTo {
    assertEquals(nonEmpty, empty.emptyTo(nonEmpty))
    assertEquals(Map(3 -> 4), Map(3 -> 4).emptyTo(nonEmpty))
  }

  @Test def entryForMaxKey {
    assertEquals(None, Map.empty[Int, String].entryForMaxKey)
    assertEquals(Some((2, "max")), Map(1 -> "min", 2 -> "max").entryForMaxKey)
  }

  @Test def entryForMinKey {
    assertEquals(None, Map.empty[Int, String].entryForMinKey)
    assertEquals(Some((1 -> "min")), Map(1 -> "min", 2 -> "max").entryForMinKey)
  }

  @Test def entryForMaxValue {
    assertEquals(None, Map.empty[Int, String].entryForMaxValue)
    assertEquals(Some((2, "def")), Map(1 -> "abc", 2 -> "def").entryForMaxValue)
  }

  @Test def entryForMinValue {
    assertEquals(None, Map.empty[Int, String].entryForMinValue)
    assertEquals(Some((1, "abc")), Map(1 -> "abc", 2 -> "def").entryForMinValue)
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
    assertEquals(Some(1), Map(1 -> "abc", 2 -> "def").keyForMinValue)
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

  @Test def findKey {
    assertEquals(None, empty.findKey(_ => true))
    assertEquals(None, nonEmpty.findKey(_ => false))
    assertEquals(Some(1), nonEmpty.findKey(_ == 1))
  }

  @Test def findValue {
    assertEquals(None, empty.findValue(_ => true))
    assertEquals(None, nonEmpty.findValue(_ => false))
    assertEquals(Some(2), nonEmpty.findValue(_ == 2))
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

  private val (empty, nonEmpty) = (Map.empty[Int, Int], Map(1 -> 2))
}
