package pimpathon

import org.junit.Test
import scala.collection.{mutable ⇒ M}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.boolean._
import pimpathon.map._
import pimpathon.util._


class MapTest {
  @Test def containsAny(): Unit = {
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

  @Test def containsAll(): Unit = {
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

  @Test def get(): Unit = {
    Map.empty[Int, Int].get(Some(1)) === None
    Map.empty[Int, Int].get(None)    === None
    Map(1 → 2).get(None)             === None
    Map(1 → 2).get(Some(2))          === None
    Map(1 → 2).get(Some(1))          === Some(2)
  }

  @Test def getOrThrow(): Unit = {
    Map(0 → "present").getOrThrow(0, "missing")                === "present"
    Map(0 → "present").getOrThrow(0, new Exception("missing")) === "present"
    Map(0 → "present").getOrThrow(0, util.goBoom: Exception)   === "present"

    assertThrows[IllegalArgumentException]("missing")(nonEmpty.getOrThrow(0, "missing"))
    assertThrows[RuntimeException]("missing")(nonEmpty.getOrThrow(0, new RuntimeException("missing")))
  }

  @Test def uncons(): Unit =
    on(empty, nonEmpty).calling(_.uncons("empty", _ ⇒ "nonEmpty")).produces("empty", "nonEmpty")

  @Test def calcIfNonEmpty(): Unit =
    on(empty, nonEmpty).calling(_.calcIfNonEmpty(_ ⇒ "nonEmpty")).produces(None, Some("nonEmpty"))

  @Test def emptyTo(): Unit = on(empty, Map(3 → 4)).calling(_.emptyTo(nonEmpty)).produces(nonEmpty, Map(3 → 4))

  @Test def entryFor_maxKey(): Unit =
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.entryFor.maxKey).produces(None, Some(2 → "max"))

  @Test def entryFor_minKey(): Unit =
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.entryFor.minKey).produces(None, Some(1 → "min"))

  @Test def entryFor_maxValue(): Unit =
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.entryFor.maxValue).produces(None, Some(2 → "def"))

  @Test def entryFor_minValue(): Unit =
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.entryFor.minValue).produces(None, Some(1 → "abc"))

  @Test def valueFor_maxKey(): Unit =
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.valueFor.maxKey).produces(None, Some("max"))

  @Test def valueFor_minKey(): Unit =
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.valueFor.minKey).produces(None, Some("min"))

  @Test def keyFor_maxValue(): Unit =
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.keyFor.maxValue).produces(None, Some(2))

  @Test def keyFor_minValue(): Unit =
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.keyFor.minValue).produces(None, Some(1))

  @Test def mapKeysEagerly(): Unit = {
    val originalKeysSeen = ints()
    def update(v: Int) = { originalKeysSeen += v; v * 10 }

    val result = Map(1 → 1, 2 → 2).mapKeysEagerly(update)
    assertEquals("Should have iterated over the original map already", List(1, 2), originalKeysSeen.toList)
    result === Map(10 → 1, 20 → 2)
    result === Map(10 → 1, 20 → 2)
    assertEquals("Shouldn't have iterated over the original map twice", List(1, 2), originalKeysSeen.toList)
  }

  @Test def mapValuesEagerly(): Unit = {
    val originalValuesSeen = ints()
    def update(v: Int) = { originalValuesSeen += v; v * 10 }

    val result = Map(1 → 1, 2 → 2).mapValuesEagerly(update)
    assertEquals("Should have iterated over the original map already", List(1, 2), originalValuesSeen.toList)
    result === Map(1 → 10, 2 → 20)
    result === Map(1 → 10, 2 → 20)
    assertEquals("Shouldn't have iterated over the original map twice", List(1, 2), originalValuesSeen.toList)
  }

  @Test def mapEntries(): Unit =
    Map(1 → 2, 2 → 4).mapEntries(k ⇒ v ⇒ (k.toString, v.toDouble)) === Map("1" → 2.0, "2" → 4.0)

  @Test def seqMapKeys(): Unit = on(Map(2 → 4, 4 → 6), Map(1 → 3))
    .calling(_.seqMapKeys(k ⇒ (k % 2 == 0).option(k / 2))).produces(Some(Map(1 → 4, 2 → 6)), None)

  @Test def seqMapValues(): Unit = on(Map(2 → 4, 4 → 6), Map(1 → 3))
    .calling(_.seqMapValues(v ⇒ (v % 2 == 0).option(v / 2))).produces(Some(Map(2 → 2, 4 → 3)), None)

  @Test def seqMapEntries(): Unit = on(Map(2 → 4, 4 → 6), Map(1 → 3))
    .calling(_.seqMapEntries(k ⇒ v ⇒ (k % 2 == 0).option((k / 2) → (v / 2)))).produces(Some(Map(1 → 2, 2 → 3)), None)

  @Test def findKey(): Unit = {
    empty.findKey(_ ⇒ true)     === None
    nonEmpty.findKey(_ ⇒ false) === None
    nonEmpty.findKey(_ == 1)    === Some(1)
  }

  @Test def findValue(): Unit = {
    empty.findValue(_ ⇒ true)      === None
    nonEmpty.findValue(_ ⇒ false)  === None
    nonEmpty.findValue(_ == 2)     === Some(2)
  }

  @Test def entryFor_matchingKey(): Unit = {
    empty.entryFor.matchingKey(_ ⇒ true)     === None
    nonEmpty.entryFor.matchingKey(_ ⇒ false) === None
    nonEmpty.entryFor.matchingKey(_ == 1)    === Some(1 → 2)
  }

  @Test def entryFor_matchingValue(): Unit = {
    empty.entryFor.matchingValue(_ ⇒ true)     === None
    nonEmpty.entryFor.matchingValue(_ ⇒ false) === None
    nonEmpty.entryFor.matchingValue(_ == 2)    === Some(1 → 2)
  }

  @Test def filterKeysNot(): Unit = {
    empty.filterKeysNot(_ ⇒ true)           === empty
    nonEmpty.filterKeysNot(_ ⇒ true)        === empty
    nonEmpty.filterKeysNot(_ ⇒ false)       === nonEmpty
    Map(1 → 2, 2 → 3).filterKeysNot(_ == 2) === nonEmpty
  }

  @Test def filterValuesNot(): Unit = {
    empty.filterValuesNot(_ ⇒ true)           === empty
    nonEmpty.filterValuesNot(_ ⇒ true)        === empty
    nonEmpty.filterValuesNot(_ ⇒ false)       === nonEmpty
    Map(1 → 2, 2 → 3).filterValuesNot(_ == 3) === nonEmpty
  }

  @Test def filterValues(): Unit = {
    empty.filterValues(_ ⇒ true)           === empty
    nonEmpty.filterValues(_ ⇒ false)       === empty
    nonEmpty.filterValues(_ ⇒ true)        === nonEmpty
    nonEmpty.filterValues(_ ⇒ true)        === nonEmpty
    Map(1 → 2, 2 → 3).filterValues(_ == 2) === nonEmpty
  }

  @Test def keyExists(): Unit = {
    assertFalse(empty.keyExists(_ ⇒ true))
    assertFalse(nonEmpty.keyExists(_ ⇒ false))
    assertFalse(nonEmpty.keyExists(_ == 2))
    assertTrue(nonEmpty.keyExists(_ == 1))
  }

  @Test def valueExists(): Unit = {
    assertFalse(empty.valueExists(_ ⇒ true))
    assertFalse(nonEmpty.valueExists(_ ⇒ false))
    assertFalse(nonEmpty.valueExists(_ == 1))
    assertTrue(nonEmpty.valueExists(_ == 2))
  }

  @Test def containsEntry(): Unit = {
    assertFalse(empty.containsEntry(1, 2))
    assertFalse(empty.containsEntry((1, 2)))
    assertTrue(nonEmpty.containsEntry(1, 2))
    assertTrue(nonEmpty.containsEntry((1, 2)))
    assertFalse(nonEmpty.containsEntry(1, 1))
    assertFalse(nonEmpty.containsEntry((1, 1)))
    assertFalse(nonEmpty.containsEntry(2, 2))
    assertFalse(nonEmpty.containsEntry((2, 2)))
  }

  @Test def mutable(): Unit = on(Map(1 → 2)).calling(_.mutable, _.toMutable).produces(M.Map(1 → 2), M.Map(1 → 2))

  @Test def reverseToMultiMap(): Unit = Map(1 → 2, 2 → 2).reverseToMultiMap === Map(2 → Set(1, 2))

  @Test def reverse(): Unit = Map(1 → 2, 2 → 2).reverse(_.min) === Map(2 → 1)

  @Test def sorted(): Unit = Map(1 → 2, 3 → 4).sorted(Ordering.Int.reverse).toList === List(3 → 4, 1 → 2)

  @Test def sortBy(): Unit = Map(1 → 2, 3 → 4).sortBy(k ⇒ -k).toList === List(3 → 4, 1 → 2)

  @Test def andThenM(): Unit =
    Map(1 → 10, 2 → 20, 3 → 30).andThenM(Map(10 → 100, 20 → 200, 40 → 400)) === Map(1 → 100, 2 → 200)

  @Test def composeM(): Unit =
    Map(10 → 100, 20 → 200, 40 → 400).composeM(Map(1 → 10, 2 → 20, 3 → 30)) === Map(1 → 100, 2 → 200)

  @Test def partitionKeys(): Unit =
    Map(1 → 2, 2 → 3).partitionKeys(_ == 1) === (Map(1 → 2), Map(2 → 3))

  @Test def partitionKeysBy(): Unit =
    Map(1 → 2, 2 → 3).partitionKeysBy { case 1 ⇒ "foo" } === (Map(2 → 3), Map("foo" → 2))

  @Test def partitionValuesBy(): Unit =
    Map(1 → 2, 2 → 3).partitionValuesBy { case 2 ⇒ "foo" } === (Map(2 → 3), Map(1 → "foo"))

  @Test def partitionEntriesBy(): Unit =
    Map(1 → 2, 2 → 3).partitionEntriesBy { case (2, 3) ⇒ "foo" → "oof" } === (Map(1 → 2), Map("foo" → "oof"))

  @Test def collectKeys(): Unit = Map(1 → 2, 2 → 3).collectKeys { case 2 ⇒ "foo" } === Map("foo" → 3)

  @Test def collectValues(): Unit = Map(1 → 2, 2 → 3).collectValues { case 2 ⇒ "foo" } === Map(1 → "foo")

  @Test def updateValue(): Unit = on(nonEmpty).calling(
    _.updateValue(1, _ ⇒None), _.updateValue(1, _ ⇒Some(1)), _.updateValue(2, _ ⇒None), _.updateValue(2, _ ⇒Some(3))
  ).produces(empty, Map(1 → 1), nonEmpty, nonEmpty)

  @Test def updateKeys(): Unit = {
    nonEmpty.updateKeys[Int]((i: Int) ⇒ None)              === empty
    nonEmpty.updateKeys(k ⇒ Some(k * 2))                   === Map(2 → 2)
    Map(1 → 2, 2 → 3).updateKeys(k ⇒ k.filterSelf(_ == 1)) === nonEmpty
  }

  @Test def updateKeysPF(): Unit = {
    nonEmpty.updateKeys(util.partial[Int, Int]())     === empty
    nonEmpty.updateKeys(util.partial(1 → 2))          === Map(2 → 2)
    Map(1 → 2, 2 → 3).updateKeys(util.partial(1 → 1)) === nonEmpty
  }

  @Test def updateValues(): Unit = {
    nonEmpty.updateValues[Int]((i: Int) ⇒ None)              === empty
    nonEmpty.updateValues(v ⇒ Some(v * 2))                   === Map(1 → 4)
    Map(1 → 2, 2 → 3).updateValues(v ⇒ v.filterSelf(_ == 2)) === nonEmpty
  }

  @Test def updateValuesPF(): Unit = {
    nonEmpty.updateValues(util.partial[Int, Int]())     === empty
    nonEmpty.updateValues(util.partial(2 → 4))          === Map(1 → 4)
    Map(1 → 2, 2 → 3).updateValues(util.partial(2 → 2)) === nonEmpty
  }

  @Test def zipWith(): Unit = Map(1 → 2, 2 → 3).zipWith(Map(1 → 20, 3 → 30)) {
    case (Some(lhs), Some(rhs)) ⇒ lhs + rhs
    case (None,      Some(rhs)) ⇒ rhs
  } === Map(1 → 22, 3 → 30)

  private val (empty, nonEmpty) = (Map.empty[Int, Int], Map(1 → 2))
}
