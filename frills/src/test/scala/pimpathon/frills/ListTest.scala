package pimpathon.frills

import org.junit.Test
import scalaz.NonEmptyList

import pimpathon.frills.list._
import pimpathon.util._


class ListTest {
  @Test def toNel(): Unit = on(Nil, List(1)).calling(_.toNel).produces(None, Some(NonEmptyList(1)))
}