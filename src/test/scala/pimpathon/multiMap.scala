package pimpathon

import org.junit.Test
import scala.collection.{mutable ⇒ M}
import scala.collection.generic.CanBuildFrom

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.multiMap._
import pimpathon.util._


class MultiMapTest {
  @Test def multiMapCBF(): Unit = {
    val cbf = MultiMap.build[List, Int, String]
    val builder = cbf.apply()

    builder += (1 → "foo") += (1 → "bar")
    builder.reset() === Map(1 → List("foo", "bar"))
    builder.reset() === Map()
  }

  @Test def ignoreFromCBF(): Unit = on(
    new UnitCanBuildFrom[List[Int], Int],
    new UnitCanBuildFrom[List[Int], Int] with IgnoreFromCBF[List[Int], Int, Unit]
  ).calling(_.apply(), _.apply(List(1, 2, 3))).produces(
    UnitBuilder[Int]("apply()"),              UnitBuilder[Int]("apply()"),
    UnitBuilder[Int]("apply(List(1, 2, 3))"), UnitBuilder[Int]("apply()")
  )

  @Test def merge(): Unit = {
    Map(1 → List(1, 2)).merge(MultiMap.empty[List, Int, Int]) === Map(1 → List(1, 2))
    MultiMap.empty[List, Int, Int].merge(Map(1 → List(1, 2))) === Map(1 → List(1, 2))
    Map(1 → List(1)).merge(Map(1 → List(2)))                  === Map(1 → List(1, 2))
    Map(1 → List(1)).merge(Map(2 → List(2)))                  === Map(1 → List(1), 2 → List(2))
    Map(1 → Set(1)).merge(Map(1 → Set(2)))                    === Map(1 → Set(1, 2))
  }

  @Test def select(): Unit = Map(1 → List(2), 2 → List(3, 4)).select(_.head) === Map(1 → 2, 2 → 3)

  @Test def append(): Unit = {
    MultiMap.empty[List, Int, Int].append(1, List(2, 3)) === Map(1 → List(2, 3))
    Map(1 → List(2)).append(1, List(3))                  === Map(1 → List(2, 3))
    Map(1 → List(2, 3)).append(1, Nil)                   === Map(1 → List(2, 3))
  }

  @Test def multiMap_head(): Unit = on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)), MultiMap.empty[List, Int, Int]
  ).calling(_.multiMap.head).produces(Map(1 → 10, 2 → 20), Map(2 → 20), Map())

  @Test def multiMap_tail(): Unit = on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)),
    Map(1 → (Nil: List[Int])), MultiMap.empty[List, Int, Int]
  ).calling(_.multiMap.tail).produces(Map(1 → List(11)), Map(), Map(), Map())

  @Test def onlyOption(): Unit = on(
    Map(1 → Nil, 2 → List(20)), Map(1 → List(10, 11), 2 → List(20)),
    Map(1 → (Nil: List[Int])), MultiMap.empty[List, Int, Int]
  ).calling(_.onlyOption).produces(Some(Map(2 → 20)), None, None, None)

  @Test def headTailOption(): Unit = on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)),
    Map(1 → (Nil: List[Int])), MultiMap.empty[List, Int, Int]
  ).calling(_.headTailOption).produces(
    Some(Map(1 → 10, 2 → 20), Map(1 → List(11))), Some(Map(2 → 20), MultiMap.empty[List, Int, Int]),
    None, None
  )

  @Test def multiMap_values(): Unit = {
    Map(1 → List(1), 2 → List(2, 3)).multiMap.values === List(1, 2, 3)
    Map(1 →  Set(1), 2 →  Set(2, 3)).multiMap.values === Set(1, 2, 3)
  }

  @Test def multiMap_reverse(): Unit = Map(1 → List(2, 3), 2 → List(3, 4)).multiMap.reverse ===
    Map(2 → List(1), 3 → List(1, 2), 4 → List(2))

  @Test def multiMap_mapEntries(): Unit =
    Map(1 → List(10, 11), 2 → List(20, 21), 3 → List(30, 31)).multiMap.mapEntries(k ⇒ vs ⇒ (k % 2, vs)) ===
      Map(0 → List(20, 21), 1 → List(10, 11, 30, 31))

  @Test def multiMap_mapEntriesU(): Unit = assertEquals(
    Map(0 → Set(20, 21), 1 → Set(10, 11, 30, 31)),
    Map(1 → List(10, 11), 2 → List(20, 21), 3 → List(30, 31)).multiMap.mapEntriesU(k ⇒ vs ⇒ (k % 2, vs.toSet))
  )

  @Test def flatMapValues(): Unit = Map(0 → List(1, 2), 1 → List(2, 3)).flatMapValues(v ⇒ List(v, -v)) ===
    Map(0 → List(1, -1, 2, -2), 1 → List(2, -2, 3, -3))

  @Test def flatMapValuesU(): Unit = {
    assertEquals(
      Map(0 → Set(1, -1, 2, -2), 1 → Set(2, -2, 3, -3)),
      Map(0 → List(1, 2), 1 → List(2, 3)).flatMapValuesU(v ⇒ Set(v, -v))
    )

    assertEquals(
      Map(0 → List(1, -1, 2, -2), 1 → List(2, -2, 3, -3)),
      Map(0 → Vector(1, 2), 1 → Vector(2, 3)).flatMapValuesU(v ⇒ List(v, -v))
    )
  }

  @Test def pop(): Unit = on(Map(1 → List(2, 3), 2 → List(3))).calling(_.pop(1), _.pop(2), _.pop(3))
    .produces(Map(1 → List(3), 2 → List(3)), Map(1 → List(2, 3)), Map(1 → List(2, 3), 2 → List(3)))

  @Test def sequence(): Unit = Map(1 → List(10, 11), 2 → List(20, 21)).sequence ===
    List(Map(1 → 10, 2 → 20), Map(1 → 11, 2 → 21))

  @Test def sliding(): Unit = Map(1 → List(11, 12, 13), 2 → List(21, 22, 23)).multiMap.sliding(2) ===
    List(Map(1 → List(11, 12), 2 → List(21, 22)), Map(1 → List(12, 13), 2 → List(22, 23)))

  @Test def getOrEmpty(): Unit = {
    on(Map(1 → List(2))).calling(_.getOrEmpty(1), _.getOrEmpty(2)).produces(List(2), Nil)
     on(Map(1 → Set(2))).calling(_.getOrEmpty(1), _.getOrEmpty(2)).produces(Set(2), Set())
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