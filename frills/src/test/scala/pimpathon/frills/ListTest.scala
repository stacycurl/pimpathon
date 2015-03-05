package pimpathon.frills

import org.junit.Test
import scalaz.NonEmptyList

import org.junit.Assert._
import pimpathon.frills.list._


class ListTest {
  @Test def toNel(): Unit = assertEquals(
    List(None, Some(NonEmptyList(1))), List(Nil, List(1)).map(_.toNel)
  )
}