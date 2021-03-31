package pimpathon

import org.junit.Assert._
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

  "amass" in Set(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ Set(i, -i) } ≡ Set(2, -2, 4, -4)
}