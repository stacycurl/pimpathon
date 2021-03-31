package pimpathon.frills

import pimpathon.PSpec
import pimpathon.frills.list._
import scalaz.{NonEmptyList, \/}

import scalaz.syntax.either._


class ListSpec extends PSpec {
  "toNel" in on(nil[Int], List(1)).calling(_.toNel).produces(None, Some(NonEmptyList(1)))

  "onlyDisjunction" in
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyDisjunction).produces(nil[Int].left, List(1, 2).left, 1.right)

  "partitionDisjunctions" in
    list(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List] ≡ (List(1, 2), List("abc", "def"))
  
  private def list(elements: (Int \/ String)*): List[Int \/ String] = elements.toList 
}