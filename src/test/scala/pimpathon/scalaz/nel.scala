package pimpathon.scalaz

import pimpathon.PSpec
import pimpathon.multiMap._
import pimpathon.scalaz.nel._
import scalaz.{NonEmptyList, \/}

import scalaz.syntax.either._


class NelSpec extends PSpec {
  "unique" in NonEmptyList(1, 2, 1).unique ≡ NonEmptyList(1, 2)

  "uniqueBy" in NonEmptyList("foo", "bar", "bard", "food", "foody", "bardo").uniqueBy(_.length) ≡
    NonEmptyList("foo", "bard", "foody")

  "filter" in
    on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.filter(_ % 2 == 0)).produces(None, Some(NonEmptyList(2)))

  "filterNot" in
    on(NonEmptyList(2), NonEmptyList(1, 2)).calling(_.filterNot(_ % 2 == 0)).produces(None, Some(NonEmptyList(1)))

  "max" in NonEmptyList(1, 3, 2).max(scalaz.std.anyVal.intInstance) ≡ 3
  "min" in NonEmptyList(3, 1, 2).min(scalaz.std.anyVal.intInstance) ≡ 1

  "partitionDisjunctions" in {
    def nel(head: (Int \/ String), tail: (Int \/ String)*): NonEmptyList[Int \/ String] =
      NonEmptyList.fromSeq(head, tail)
    
    nel(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List] ≡ (List(1, 2), List("abc", "def"))
  }

  "partitionEithers" in {
    NonEmptyList[Either[Int, String]](Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List] ≡
      (List(1, 2), List("abc", "def"))
  }

  "toMultiMap" in on(NonEmptyList((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  "asMultiMap_withKeys" in {
    on(NonEmptyList(0, 1, 2, 3))
      .calling(_.asMultiMap[List].withKeys(_ % 2))
      .produces(Map(0 → List(0, 2), 1 → List(1, 3)))

    on(NonEmptyList(0, 1, 2, 3))
      .calling(_.asMultiMap[NonEmptyList].withKeys(_ % 2))
      .produces(Map(0 → NonEmptyList(0, 2), 1 → NonEmptyList(1, 3)))
  }

  "onlyOption" in on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.onlyOption).produces(Some(1), None)

  "onlyEither" in
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyEither).produces(Left(NonEmptyList(1, 2)), Right(1))

  "onlyDisjunction" in
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyDisjunction).produces(NonEmptyList(1, 2).left, 1.right)

  "onlyOrDisjunction" in
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyOrDisjunction(_.size)).produces(2.left, 1.right)
}