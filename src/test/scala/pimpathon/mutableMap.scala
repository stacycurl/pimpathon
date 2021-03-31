package pimpathon

import pimpathon.mutableMap._

import scala.collection.{mutable => M}


class MutableMapSpec extends PSpec {
  "retainKeys" in {
    empty.retainKeys(_ ⇒ false)            ≡ empty
    nonEmpty.retainKeys(_ ⇒ false)         ≡ empty
    nonEmpty.retainKeys(_ ⇒ true)          ≡ nonEmpty
    M.Map(1 → 2, 2 → 3).retainKeys(_ == 1) ≡ nonEmpty
  }

  "retainValues" in {
    empty.retainValues(_ ⇒ false)            ≡ empty
    nonEmpty.retainValues(_ ⇒ false)         ≡ empty
    nonEmpty.retainValues(_ ⇒ true)          ≡ nonEmpty
    M.Map(1 → 2, 2 → 3).retainValues(_ == 2) ≡ nonEmpty
  }

  private def empty    = M.Map[Int, Int]()
  private def nonEmpty = M.Map[Int, Int](1 → 2)
}