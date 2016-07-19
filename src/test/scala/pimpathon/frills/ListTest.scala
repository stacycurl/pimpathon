package pimpathon.frills

import org.junit.Test
import scalaz.NonEmptyList

import pimpathon.frills.list._
import pimpathon.util._
import scalaz.syntax.either._


class ListTest {
  @Test def toNel(): Unit = on(Nil, List(1)).calling(_.toNel).produces(None, Some(NonEmptyList(1)))

  @Test def onlyDisjunction(): Unit =
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyDisjunction).produces(Nil.left, List(1, 2).left, 1.right)

  @Test def partitionDisjunctions(): Unit =
    List(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List] === (List(1, 2), List("abc", "def"))
}