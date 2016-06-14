package pimpathon.scalaz

import org.junit.Test
import pimpathon.util.on
import scalaz.NonEmptyList

import pimpathon.multiMap._
import pimpathon.scalaz.nel._
import pimpathon.util._
import scalaz.syntax.either._


class NelTest {
  @Test def distinct(): Unit = NonEmptyList(1, 2, 1).distinct === NonEmptyList(1, 2)

  @Test def distinctBy(): Unit = NonEmptyList("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length) ===
    NonEmptyList("foo", "bard", "foody")

  @Test def filter(): Unit =
    on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.filter(_ % 2 == 0)).produces(None, Some(NonEmptyList(2)))

  @Test def filterNot(): Unit =
    on(NonEmptyList(2), NonEmptyList(1, 2)).calling(_.filterNot(_ % 2 == 0)).produces(None, Some(NonEmptyList(1)))

  @Test def max(): Unit = NonEmptyList(1, 3, 2).max(scalaz.std.anyVal.intInstance) === 3
  @Test def min(): Unit = NonEmptyList(3, 1, 2).min(scalaz.std.anyVal.intInstance) === 1

  @Test def partitionDisjunctions(): Unit =
    NonEmptyList(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List] ===
      (List(1, 2), List("abc", "def"))

  @Test def partitionEithers(): Unit =
    NonEmptyList(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List] ===
      (List(1, 2), List("abc", "def"))

  @Test def toMultiMap(): Unit = on(NonEmptyList((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  @Test def asMultiMap_withKeys(): Unit = on(NonEmptyList(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[NonEmptyList].withKeys(_ % 2))
    .produces(Map(0 → List(0, 2), 1 → List(1, 3)), Map(0 → NonEmptyList(0, 2), 1 → NonEmptyList(1, 3)))

  @Test def onlyOption(): Unit = on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.onlyOption).produces(Some(1), None)

  @Test def onlyEither(): Unit =
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyEither).produces(Left(NonEmptyList(1, 2)), Right(1))

  @Test def onlyDisjunction(): Unit =
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyDisjunction).produces(NonEmptyList(1, 2).left, 1.right)

  @Test def onlyOrDisjunction(): Unit =
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyOrDisjunction(_.size)).produces(2.left, 1.right)
}