package pimpathon

import org.junit.Assert._
import pimpathon.multiMap._
import pimpathon.set._

import scala.collection.{mutable => M}


class SetSpec extends PSpec {
  "notContains" in {
    assertTrue(Set.empty[Int].notContains(3))
    assertFalse(Set(3).notContains(3))
  }

  "powerSet" in {
    Set.empty[Int].powerSet ≡ Set(Set.empty[Int])
    Set(1).powerSet         ≡ Set(Set.empty[Int], Set(1))
    Set(1, 2).powerSet      ≡ Set(Set.empty[Int], Set(1), Set(2), Set(1, 2))
  }

  "sorted" in Set(4, 1, 2).sorted.toList ≡ List(1, 2, 4)

  "mutable" in on(Set(1, 2)).calling(_.mutable, _.toMutable).produces(M.Set(1, 2), M.Set(1, 2))

  "partitionEithers" in
    Set(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[Set] ≡ (Set(1, 2), Set("abc", "def"))

  "onlyOrThrow" in {
    on(Set.empty[Int], Set(1, 2)).calling(_.onlyOrThrow(s ⇒ exception(s"${s.size} elements"))).throws("0 elements", "2 elements")
    Set(1).onlyOrThrow(_ ⇒ new Exception()) ≡ 1
  }

  "onlyEither" in
    on(Set.empty[Int], Set(1, 2), Set(1)).calling(_.onlyEither).produces(Left(Set.empty[Int]), Left(Set(1, 2)), Right(1))

  "onlyOption" in on(Set.empty[Int], Set(1, 2), Set(1)).calling(_.onlyOption).produces(None, None, Some(1))

  "toMultiMap" in on(Set((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  "asMultiMap_withKeys" in on(Set(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[Set].withKeys(_ % 2))
    .produces(Map(0 → List(0, 2), 1 → List(1, 3)), Map(0 → Set(0, 2), 1 → Set(1, 3)))

  "amass" in Set(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ Set(i, -i) } ≡ Set(2, -2, 4, -4)
}