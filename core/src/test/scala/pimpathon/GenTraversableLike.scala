package pimpathon

import org.junit.Test
import scala.collection.immutable.SortedMap

import org.junit.Assert._
import pimpathon.boolean._
import pimpathon.genTraversableLike._
import pimpathon.multiMap._
import pimpathon.util._


class GenTraversableLikeTests {
  @Test def asMultiMap_withKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 → List(0, 2), 1 → List(1, 3)), List(0, 1, 2, 3).asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 → Set(0, 2), 1 → Set(1, 3)), List(0, 1, 2, 3).asMultiMap[Set].withKeys(_ % 2))

    assertEquals(Map(), Set.empty[Int].asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 → List(0, 2), 1 → List(1, 3)), Set(0, 1, 2, 3).asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 → Set(0, 2), 1 → Set(1, 3)), Set(0, 1, 2, 3).asMultiMap[Set].withKeys(_ % 2))
  }

  @Test def asMultiMap_withValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 → List(0), 1 → List(1), 2 → List(0), 3 → List(1)),
      List(0, 1, 2, 3).asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 → Set(0), 1 → Set(1), 2 → Set(0), 3 → Set(1)),
      List(0, 1, 2, 3).asMultiMap[Set].withValues(_ % 2))


    assertEquals(Map(), Set.empty[Int].asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 → List(0), 1 → List(1), 2 → List(0), 3 → List(1)),
      Set(0, 1, 2, 3).asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 → Set(0), 1 → Set(1), 2 → Set(0), 3 → Set(1)),
      Set(0, 1, 2, 3).asMultiMap[Set].withValues(_ % 2))
  }

  @Test def asMultiMap_withSomeKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 → List(1, 3)),
      List(0, 1, 2, 3).asMultiMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i % 2)))


    assertEquals(Map(), Set.empty[Int].asMultiMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 → List(1, 3)),
      Set(0, 1, 2, 3).asMultiMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withSomeValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeValues(i ⇒ (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 → List(1), 3 → List(1)),
      List(0, 1, 2, 3).asMultiMap.withSomeValues(i ⇒ (i % 2 == 1).option(i % 2)))


    assertEquals(Map(), Set.empty[Int].asMultiMap.withSomeValues(i ⇒ (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 → List(1), 3 → List(1)),
      Set(0, 1, 2, 3).asMultiMap.withSomeValues(i ⇒ (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withEntries(): Unit = assertEquals(
    Map(1 → List((10, 100), (11, 110)), 2 → List((20, 200))),
    Map((1, 10) → 100, (1, 11) → 110, (2, 20) → 200).asMultiMap[List].withEntries { case ((a, b), c) ⇒ (a, (b, c)) }
  )

  @Test def asMultiMap_withPFKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withPFKeys { case i if i % 2 == 1 ⇒ i % 2 })

    assertEquals(Map(1 → List(1, 3)),
      List(0, 1, 2, 3).asMultiMap.withPFKeys { case i if i % 2 == 1 ⇒ i % 2 })


    assertEquals(Map(), Set.empty[Int].asMultiMap.withPFKeys { case i if i % 2 == 1 ⇒ i % 2 })

    assertEquals(Map(1 → List(1, 3)),
      Set(0, 1, 2, 3).asMultiMap.withPFKeys { case i if i % 2 == 1 ⇒ i % 2 })
  }

  @Test def asMultiMap_withPFValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withPFValues { case i if i % 2 == 1 ⇒ i % 2 })

    assertEquals(Map(1 → List(1), 3 → List(1)),
      List(0, 1, 2, 3).asMultiMap.withPFValues { case i if i % 2 == 1 ⇒ i % 2 })


    assertEquals(Map(), Set.empty[Int].asMultiMap.withPFValues { case i if i % 2 == 1 ⇒ i % 2 })

    assertEquals(Map(1 → List(1), 3 → List(1)),
      Set(0, 1, 2, 3).asMultiMap.withPFValues { case i if i % 2 == 1 ⇒ i % 2 })
  }

  @Test def asMultiMap_withManyKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMultiMap.withManyKeys(i ⇒ List(-i, i)))

    assertEquals(Map(1 → List(1), 2 → List(1, 2), 3 → List(2)),
      List(1, 2).asMultiMap.withManyKeys(i ⇒ List(i, i + 1)))


    assertEquals(Map(), Set.empty[Int].asMultiMap.withManyKeys(i ⇒ List(-i, i)))

    assertEquals(Map(1 → List(1), 2 → List(1, 2), 3 → List(2)),
      Set(1, 2).asMultiMap.withManyKeys(i ⇒ List(i, i + 1)))
  }

  @Test def asMultiMap_withUniqueKeys(): Unit = {
    assertEquals(Some(Map()), List.empty[Int].asMultiMap.withUniqueKeys(_ % 2))
    assertEquals(Some(Map(0 → List(0), 1 → List(1))), List(0, 1).asMultiMap[List].withUniqueKeys(_ % 2))
    assertEquals(None, List(0, 1, 2).asMultiMap[List].withUniqueKeys(_ % 2))
  }

  @Test def asMap_withKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withKeys(_ * 2))
    assertEquals(Map(2 → 1), List(1).asMap.withKeys(_ * 2))

    assertEquals(Map(), Set.empty[Int].asMap.withKeys(_ * 2))
    assertEquals(Map(2 → 1), Set(1).asMap.withKeys(_ * 2))
  }

  @Test def asMap_withValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withValues(_ * 2))
    assertEquals(Map(1 → 2), List(1).asMap.withValues(_ * 2))

    assertEquals(Map(), Set.empty[Int].asMap.withValues(_ * 2))
    assertEquals(Map(1 → 2), Set(1).asMap.withValues(_ * 2))
  }

  @Test def asMap_withConstValue(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withConstValue(2))
    assertEquals(Map(1 → 2, 2 → 2), List(1, 2).asMap.withConstValue(2))

    assertEquals(Map(), Set.empty[Int].asMap.withConstValue(2))
    assertEquals(Map(1 → 2, 2 → 2), Set(1, 2).asMap.withConstValue(2))
  }

  @Test def asMap_withSomeKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withSomeKeys(i ⇒ Some(i * 2)))
    assertEquals(Map(2 → 1), List(1, 2).asMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i * 2)))

    assertEquals(Map(), Set.empty[Int].asMap.withSomeKeys(i ⇒ Some(i * 2)))
    assertEquals(Map(2 → 1), Set(1, 2).asMap.withSomeKeys(i ⇒ (i % 2 == 1).option(i * 2)))
  }

  @Test def asMap_withSomeValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withSomeValues(i ⇒ Some(i * 2)))
    assertEquals(Map(1 → 2), List(1, 2).asMap.withSomeValues(i ⇒ (i % 2 == 1).option(i * 2)))

    assertEquals(Map(), Set.empty[Int].asMap.withSomeValues(i ⇒ Some(i * 2)))
    assertEquals(Map(1 → 2), Set(1, 2).asMap.withSomeValues(i ⇒ (i % 2 == 1).option(i * 2)))
  }

  @Test def asMap_withEntries(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withEntries(entriesFn))
    assertEquals(Map(1 → 4, 2 → 8), List(2, 4).asMap.withEntries(entriesFn))

    assertEquals(Map(), Set.empty[Int].asMap.withEntries(entriesFn))
    assertEquals(Map(1 → 4, 2 → 8), Set(2, 4).asMap.withEntries(entriesFn))
  }

  @Test def asMap_withSomeEntries(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withSomeEntries(entriesPF.lift))
    assertEquals(Map(0 → 2, 1 → 6), List(1, 2, 3).asMap.withSomeEntries(entriesPF.lift))

    assertEquals(Map(), Set.empty[Int].asMap.withSomeEntries(entriesPF.lift))
    assertEquals(Map(0 → 2, 1 → 6), List(1, 2, 3).asMap.withSomeEntries(entriesPF.lift))
  }

  @Test def asMap_withPFKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withPFKeys { case i ⇒ i * 2 })
    assertEquals(Map(2 → 1), List(1, 2).asMap.withPFKeys { case i if i % 2 == 1 ⇒ i * 2 })

    assertEquals(Map(), Set.empty[Int].asMap.withPFKeys { case i ⇒ i * 2 })
    assertEquals(Map(2 → 1), Set(1, 2).asMap.withPFKeys { case i if i % 2 == 1 ⇒ i * 2 })
  }

  @Test def asMap_withPFValues(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withPFValues { case i ⇒ i * 2 })
    assertEquals(Map(1 → 2), List(1, 2).asMap.withPFValues { case i if i % 2 == 1 ⇒ i * 2 })

    assertEquals(Map(), Set.empty[Int].asMap.withPFValues { case i ⇒ i * 2 })
    assertEquals(Map(1 → 2), Set(1, 2).asMap.withPFValues { case i if i % 2 == 1 ⇒ i * 2 })
  }

  @Test def asMap_withPFEntries(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withPFEntries(entriesPF))
    assertEquals(Map(0 → 2, 1 → 6), List(1, 2, 3).asMap.withPFEntries(entriesPF))

    assertEquals(Map(), Set.empty[Int].asMap.withPFEntries(entriesPF))
    assertEquals(Map(0 → 2, 1 → 6), List(1, 2, 3).asMap.withPFEntries(entriesPF))
  }

  @Test def as_SortedMap_withValues(): Unit = {
    assertEquals(Map(), List.empty[Int].as[SortedMap].withValues(_ * 2))
    assertEquals(Map(1 → 2), List(1).as[SortedMap].withValues(_ * 2))

    assertEquals(Map(), Set.empty[Int].as[SortedMap].withValues(_ * 2))
    assertEquals(Map(1 → 2), Set(1).as[SortedMap].withValues(_ * 2))
  }

  @Test def asMap_withManyKeys(): Unit = {
    assertEquals(Map(), List.empty[Int].asMap.withManyKeys(i ⇒ List(-i, i)))

    assertEquals(Map(-2 → 2, -1 → 1, 1 → 1, 2 → 2),
      Set(1, 2).asMap.withManyKeys(i ⇒ List(-i, i)))


    assertEquals(Map(), Set.empty[Int].asMap.withManyKeys(i ⇒ List(-i, i)))

    assertEquals(Map(-2 → 2, -1 → 1, 1 → 1, 2 → 2),
      Set(1, 2).asMap.withManyKeys(i ⇒ List(-i, i)))
  }

  @Test def asMap_withUniqueKeys(): Unit = {
    assertEquals(None, List(1, 2, 3).asMap.withUniqueKeys(_ % 2))
    assertEquals(Some(Map(0 → 2, 1 → 1)), List(1, 2).asMap.withUniqueKeys(_ % 2))
    assertEquals(Some(Map()), nil[Int].asMap.withUniqueKeys(_ % 2))
  }

  @Test def attributeCounts(): Unit = {
    assertEquals(Map(3 → 2, 4 → 1), List("foo", "food", "bar").attributeCounts(_.size))

    assertEquals(Map(3 → 2, 4 → 1), Set("foo", "food", "bar").attributeCounts(_.size))
  }

  @Test def optAttributeCounts(): Unit = {
    import pimpathon.any._

    assertEquals(Map(3 → 2, 4 → 1),
      List("foo", "food", "bar", "oo").optAttributeCounts(_.size.filterSelf(_ > 2)))

    assertEquals(Map(3 → 2, 4 → 1),
      Set("foo", "food", "bar", "oo").optAttributeCounts(_.size.filterSelf(_ > 2)))
  }

  @Test def collectAttributeCounts(): Unit = {
    assertEquals(Map(3 → 2, 4 → 1),
      List("foo", "food", "bar", "oo").collectAttributeCounts { case word if word.size > 2 ⇒ word.size })

    assertEquals(Map(3 → 2, 4 → 1),
      Set("foo", "food", "bar", "oo").collectAttributeCounts { case word if word.size > 2 ⇒ word.size })
  }

  @Test def toMultiMap(): Unit = {
    assertEquals(Map(), Set.empty[(Int, Int)].toMultiMap[List])
    assertEquals(Map(), List.empty[(Int, Int)].toMultiMap[List])
    assertEquals(Map(), List.empty[(Int, Int)].toMultiMap[List])


    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])

    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])


    assertEquals(Map(1 → Set(10, 11), 2 → Set(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])

    assertEquals(Map(1 → Set(10, 11), 2 → Set(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])
  }

  @Test def all(): Unit = {
    on(Nil, List(true), List(false), List(true, true), List(false, true))
      .calling(_.all(true)).produces(true, true, false, true, false)

    on(Set.empty[Int], List(1), List(2), List(1, 1), List(2, 1))
      .calling(_.all(1)).produces(true, true, false, true, false)
  }

  @Test def seqMap(): Unit = {
    assertEquals(Some(Nil),      nil[Int].seqMap(_ ⇒ Some(1)))
    assertEquals(None,           List(1).seqMap(_ ⇒ None))
    assertEquals(Some(List(1)),  List(1).seqMap(Some(_)))

    assertEquals(None, List(1, 2, 3).seqMap {
      case 1 ⇒ Some(1)
      case 2 ⇒ None
      case 3 ⇒ goBoom
    })


    assertEquals(Some(Nil),        Vector.empty[Int].seqMap(_ ⇒ Some(1)))
    assertEquals(None,             Vector(1).seqMap(_ ⇒ None))
    assertEquals(Some(Vector(1)),  Vector(1).seqMap(Some(_)))

    assertEquals(None, Vector(1, 2, 3).seqMap {
      case 1 ⇒ Some(1)
      case 2 ⇒ None
      case 3 ⇒ goBoom
    })
  }

  @Test def seqFold(): Unit = {
    assertEquals(None, List(1, 2, 3).seqFold(0) {
      case (0, j) ⇒ Some(0 + j)
      case (1, _) ⇒ None
      case _      ⇒ goBoom
    })

    assertEquals(Some(6), List(1, 2, 3).seqFold(0) { case (i, j) ⇒ Some(i + j) })
  }

  @Test def apoFold(): Unit = {
    assertEquals(Left(3), List(1, 2, 3).apoFold(0) {
      case (0, j) ⇒ Right(0 + j)
      case (1, j) ⇒ Left(1 + j)
      case _      ⇒ goBoom
    })

    assertEquals(Right(6), List(1, 2, 3).apoFold(0) { case (i, j) ⇒ Right(i + j) })
  }

  private val entriesPF: PartialFunction[Int, (Int, Int)] = { case i if i % 2 == 1 ⇒ (i/2, i*2) }
  private val entriesFn: Int ⇒ (Int, Int) = (i: Int) ⇒ (i/2, i*2)
}