package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.TreeMap
import scala.reflect.ClassManifest
import scala.util.control._

import org.junit.Assert._
import pimpathon.map._


class MapTest {
  @Test def containsAny: Unit = {
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

  @Test def containsAll: Unit = {
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

  @Test def get: Unit = {
    assertEquals(None,    Map.empty[Int, Int].get(Some(1)))
    assertEquals(None,    Map.empty[Int, Int].get(None))
    assertEquals(None,    Map(1 -> 2).get(None))
    assertEquals(None,    Map(1 -> 2).get(Some(2)))
    assertEquals(Some(2), Map(1 -> 2).get(Some(1)))
  }

  @Test def getOrThrow: Unit = {
    assertEquals("present", Map(0 -> "present").getOrThrow(0, "missing"))
    assertEquals("present", Map(0 -> "present").getOrThrow(0, new Exception("missing")))
    assertEquals("present", Map(0 -> "present").getOrThrow(0, util.goBoom: Exception))

    assertEquals("missing", util.intercept[IllegalArgumentException] {
      nonEmpty.getOrThrow(0, "missing")
    }.getMessage)

    assertEquals("missing", util.intercept[RuntimeException] {
      nonEmpty.getOrThrow(0, new RuntimeException("missing"))
    }.getMessage)
  }

  @Test def uncons: Unit = {
    assertEquals("empty", empty.uncons("empty", _ => "nonEmpty"))
    assertEquals("nonEmpty", nonEmpty.uncons("empty", _ => "nonEmpty"))
  }

  @Test def mapNonEmpty: Unit = {
    assertEquals(None, empty.mapNonEmpty(_ => "nonEmpty"))
    assertEquals(Some("nonEmpty"), nonEmpty.mapNonEmpty(_ => "nonEmpty"))
  }

  @Test def emptyTo: Unit = {
    assertEquals(nonEmpty, empty.emptyTo(nonEmpty))
    assertEquals(Map(3 -> 4), Map(3 -> 4).emptyTo(nonEmpty))
  }

  @Test def entryFor_maxKey: Unit = {
    assertEquals(None, Map.empty[Int, String].entryFor.maxKey)
    assertEquals(Some(2 -> "max"), Map(1 -> "min", 2 -> "max").entryFor.maxKey)
  }

  @Test def entryFor_minKey: Unit = {
    assertEquals(None, Map.empty[Int, String].entryFor.minKey)
    assertEquals(Some(1 -> "min"), Map(1 -> "min", 2 -> "max").entryFor.minKey)
  }

  @Test def entryFor_maxValue: Unit = {
    assertEquals(None, Map.empty[Int, String].entryFor.maxValue)
    assertEquals(Some(2 -> "def"), Map(1 -> "abc", 2 -> "def").entryFor.maxValue)
  }

  @Test def entryFor_minValue: Unit = {
    assertEquals(None, Map.empty[Int, String].entryFor.minValue)
    assertEquals(Some(1 -> "abc"), Map(1 -> "abc", 2 -> "def").entryFor.minValue)
  }

  @Test def valueFor_maxKey: Unit = {
    assertEquals(None, Map.empty[Int, String].valueFor.maxKey)
    assertEquals(Some("max"), Map(1 -> "min", 2 -> "max").valueFor.maxKey)
  }

  @Test def valueFor_minKey: Unit = {
    assertEquals(None, Map.empty[Int, String].valueFor.minKey)
    assertEquals(Some("min"), Map(1 -> "min", 2 -> "max").valueFor.minKey)
  }

  @Test def keyFor_maxValue: Unit = {
    assertEquals(None, Map.empty[Int, String].keyFor.maxValue)
    assertEquals(Some(2), Map(1 -> "abc", 2 -> "def").keyFor.maxValue)
  }

  @Test def keyFor_minValue: Unit = {
    assertEquals(None, Map.empty[Int, String].keyFor.minValue)
    assertEquals(Some(1), Map(1 -> "abc", 2 -> "def").keyFor.minValue)
  }

  @Test def mapKeysEagerly: Unit = {
    val originalKeysSeen = new M.ListBuffer[Int]
    def update(v: Int) = { originalKeysSeen += v; v * 10 }

    val result = Map(1 -> 1, 2 -> 2).mapKeysEagerly(update)
    assertEquals("Should have iterated over the original map already", List(1, 2), originalKeysSeen.toList)
    assertEquals(Map(10 -> 1, 20 -> 2), result)
    assertEquals(Map(10 -> 1, 20 -> 2), result)
    assertEquals("Shouldn't have iterated over the original map twice", List(1, 2), originalKeysSeen.toList)
  }

  @Test def mapValuesEagerly: Unit = {
    val originalValuesSeen = new M.ListBuffer[Int]
    def update(v: Int) = { originalValuesSeen += v; v * 10 }

    val result = Map(1 -> 1, 2 -> 2).mapValuesEagerly(update)
    assertEquals("Should have iterated over the original map already", List(1, 2), originalValuesSeen.toList)
    assertEquals(Map(1 -> 10, 2 -> 20), result)
    assertEquals(Map(1 -> 10, 2 -> 20), result)
    assertEquals("Shouldn't have iterated over the original map twice", List(1, 2), originalValuesSeen.toList)
  }

  @Test def findKey: Unit = {
    assertEquals(None, empty.findKey(_ => true))
    assertEquals(None, nonEmpty.findKey(_ => false))
    assertEquals(Some(1), nonEmpty.findKey(_ == 1))
  }

  @Test def findValue: Unit = {
    assertEquals(None, empty.findValue(_ => true))
    assertEquals(None, nonEmpty.findValue(_ => false))
    assertEquals(Some(2), nonEmpty.findValue(_ == 2))
  }

  @Test def entryFor_matchingKey: Unit = {
    assertEquals(None, empty.entryFor.matchingKey(_ => true))
    assertEquals(None, nonEmpty.entryFor.matchingKey(_ => false))
    assertEquals(Some(1 -> 2), nonEmpty.entryFor.matchingKey(_ == 1))
  }

  @Test def entryFor_matchingValue: Unit = {
    assertEquals(None, empty.entryFor.matchingValue(_ => true))
    assertEquals(None, nonEmpty.entryFor.matchingValue(_ => false))
    assertEquals(Some(1 -> 2), nonEmpty.entryFor.matchingValue(_ == 2))
  }

  @Test def filterKeysNot: Unit = {
    assertEquals(empty, empty.filterKeysNot(_ => true))
    assertEquals(empty, nonEmpty.filterKeysNot(_ => true))
    assertEquals(nonEmpty, nonEmpty.filterKeysNot(_ => false))
    assertEquals(nonEmpty, Map(1 -> 2, 2 -> 3).filterKeysNot(_ == 2))
  }

  @Test def filterValuesNot: Unit = {
    assertEquals(empty, empty.filterValuesNot(_ => true))
    assertEquals(empty, nonEmpty.filterValuesNot(_ => true))
    assertEquals(nonEmpty, nonEmpty.filterValuesNot(_ => false))
    assertEquals(nonEmpty, Map(1 -> 2, 2 -> 3).filterValuesNot(_ == 3))
  }

  @Test def filterValues: Unit = {
    assertEquals(empty, empty.filterValues(_ => true))
    assertEquals(empty, nonEmpty.filterValues(_ => false))
    assertEquals(nonEmpty, nonEmpty.filterValues(_ => true))
    assertEquals(nonEmpty, nonEmpty.filterValues(_ => true))
    assertEquals(nonEmpty, Map(1 -> 2, 2 -> 3).filterValues(_ == 2))
  }

  @Test def valueExists: Unit = {
    assertFalse(empty.valueExists(_ => true))
    assertFalse(nonEmpty.valueExists(_ => false))
    assertTrue(nonEmpty.valueExists(_ == 2))
  }

  @Test def mutable: Unit = {
    assertEquals(M.Map(1 -> 2), Map(1 -> 2).mutable)
    assertEquals(M.Map(1 -> 2), Map(1 -> 2).toMutable)
  }

  @Test def reverseToMultiMap: Unit = {
    assertEquals(Map(2 -> Set(1, 2)), Map(1 -> 2, 2 -> 2).reverseToMultiMap)
  }

  @Test def reverse: Unit = {
    assertEquals(Map(2 -> 1), Map(1 -> 2, 2 -> 2).reverse(_.min))
  }

  @Test def sorted: Unit = {
    assertEquals(List(3 -> 4, 1 -> 2), Map(1 -> 2, 3 -> 4).sorted(Ordering.Int.reverse).toList)
  }

  @Test def andThenM: Unit = {
    assertEquals(Map(1 -> 100, 2 -> 200),
      Map(1 -> 10, 2 -> 20, 3 -> 30).andThenM(Map(10 -> 100, 20 -> 200, 40 -> 400)))
  }

  @Test def partitionKeysBy: Unit = assertEquals(
    (Map("foo" -> 2), Map(2 -> 3)),
    Map(1 -> 2, 2 -> 3).partitionKeysBy { case 1 => "foo" }
  )

  @Test def partitionValuesBy: Unit = assertEquals(
    (Map(1 -> "foo"), Map(2 -> 3)),
    Map(1 -> 2, 2 -> 3).partitionValuesBy { case 2 => "foo" }
  )
  private val (empty, nonEmpty) = (Map.empty[Int, Int], Map(1 -> 2))
}
