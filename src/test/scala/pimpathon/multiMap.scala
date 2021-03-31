package pimpathon

import pimpathon.builder._
import pimpathon.multiMap._

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable => M}


class MultiMapSpec extends PSpec {
  "multiMapCBF" in {
    val cbf = MultiMap.build[List, Int, String]
    val builder = cbf.apply()

    builder += (1 → "foo") += (1 → "bar")
    builder.reset() ≡ Map(1 → List("foo", "bar"))
    builder.reset() ≡ Map()
  }

  "ignoreFromCBF" in on(
    new UnitCanBuildFrom[List[Int], Int],
    new UnitCanBuildFrom[List[Int], Int] with IgnoreFromCBF[List[Int], Int, Unit]
  ).calling(_.apply(), _.apply(List(1, 2, 3))).produces(
    UnitBuilder[Int]("apply()"),              UnitBuilder[Int]("apply()"),
    UnitBuilder[Int]("apply(List(1, 2, 3))"), UnitBuilder[Int]("apply()")
  )

  "merge" in {
    Map(1 → List(1, 2)).merge(MultiMap.empty[List, Int, Int]) ≡ Map(1 → List(1, 2))
    MultiMap.empty[List, Int, Int].merge(Map(1 → List(1, 2))) ≡ Map(1 → List(1, 2))
    Map(1 → List(1)).merge(Map(1 → List(2)))                  ≡ Map(1 → List(1, 2))
    Map(1 → List(1)).merge(Map(2 → List(2)))                  ≡ Map(1 → List(1), 2 → List(2))
    Map(1 → Set(1)).merge(Map(1 → Set(2)))                    ≡ Map(1 → Set(1, 2))
  }

  "select" in Map(1 → List(2), 2 → List(3, 4)).select(_.head) ≡ Map(1 → 2, 2 → 3)

  "append" in {
    MultiMap.empty[List, Int, Int].append(1, List(2, 3)) ≡ Map(1 → List(2, 3))
    Map(1 → List(2)).append(1, List(3))                  ≡ Map(1 → List(2, 3))
    Map(1 → List(2, 3)).append(1, Nil)                   ≡ Map(1 → List(2, 3))
  }

  "multiMap_head" in on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)), MultiMap.empty[List, Int, Int]
  ).calling(_.multiMap.head).produces(Map(1 → 10, 2 → 20), Map(2 → 20), Map())

  "multiMap_tail" in on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)), Map(1 → Nil), MultiMap.empty[List, Int, Int]
  ).calling(_.multiMap.tail).produces(Map(1 → List(11)), Map(), Map(), Map())

  "onlyOption" in on(
    Map(1 → Nil, 2 → List(20)), Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil), MultiMap.empty[List, Int, Int]
  ).calling(_.onlyOption).produces(Some(Map(2 → 20)), None, None, None)

  "headTailOption" in on(
    Map(1 → List(10, 11), 2 → List(20)), Map(1 → Nil, 2 → List(20)), Map(1 → Nil), MultiMap.empty[List, Int, Int]
  ).calling(_.headTailOption).produces(
    Some(Map(1 → 10, 2 → 20), Map(1 → List(11))), Some(Map(2 → 20), MultiMap.empty[List, Int, Int]), None, None
  )

  "multiMap_values" in {
    Map(1 → List(1), 2 → List(2, 3)).multiMap.values ≡ List(1, 2, 3)
    Map(1 →  Set(1), 2 →  Set(2, 3)).multiMap.values ≡ Set(1, 2, 3)
  }

  "multiMap_reverse" in Map(1 → List(2, 3), 2 → List(3, 4)).multiMap.reverse ≡
    Map(2 → List(1), 3 → List(1, 2), 4 → List(2))

  "multiMap_mapEntries" in
    Map(1 → List(10, 11), 2 → List(20, 21), 3 → List(30, 31)).multiMap.mapEntries(k ⇒ vs ⇒ (k % 2, vs)) ≡
      Map(0 → List(20, 21), 1 → List(10, 11, 30, 31))

  "multiMap_mapEntriesU" in
    Map(1 → List(10, 11), 2 → List(20, 21), 3 → List(30, 31)).multiMap.mapEntriesU(k ⇒ vs ⇒ (k % 2, vs.toSet)) ≡
      Map(0 → Set(20, 21), 1 → Set(10, 11, 30, 31))

  "multiMap_mapValues" in
    Map(1 → List(10, 11), 2 → List(20, 21), 3 → List(30, 31)).multiMap.mapValues(v ⇒ v * 2) ≡
      Map(1 → List(20, 22), 2 → List(40, 42), 3 → List(60, 62))

  "flatMapValues" in Map(0 → List(1, 2), 1 → List(2, 3)).flatMapValues(v ⇒ List(v, -v)) ≡
    Map(0 → List(1, -1, 2, -2), 1 → List(2, -2, 3, -3))

  "flatMapValuesU" in {
    Map(0 → List(1, 2), 1 → List(2, 3)).flatMapValuesU(v ⇒ Set(v, -v)) ≡
      Map(0 → Set(1, -1, 2, -2), 1 → Set(2, -2, 3, -3))

    Map(0 → Vector(1, 2), 1 → Vector(2, 3)).flatMapValuesU(v ⇒ List(v, -v)) ≡
      Map(0 → List(1, -1, 2, -2), 1 → List(2, -2, 3, -3))
  }

  "pop" in on(Map(1 → List(2, 3), 2 → List(3))).calling(_.pop(1), _.pop(2), _.pop(3))
    .produces(Map(1 → List(3), 2 → List(3)), Map(1 → List(2, 3)), Map(1 → List(2, 3), 2 → List(3)))

  "sequence" in Map(1 → List(10, 11), 2 → List(20, 21)).sequence ≡
    List(Map(1 → 10, 2 → 20), Map(1 → 11, 2 → 21))

  "sliding" in Map(1 → List(11, 12, 13), 2 → List(21, 22, 23)).multiMap.sliding(2) ≡
    List(Map(1 → List(11, 12), 2 → List(21, 22)), Map(1 → List(12, 13), 2 → List(22, 23)))

  "getOrEmpty" in {
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