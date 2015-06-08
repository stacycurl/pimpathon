package pimpathon.scalaz

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.tuple._
import pimpathon.util._
import pimpathon.scalaz.either._

import scalaz.{\/-, -\/, \/}


class DisjunctionTest {
  @Test def tap(): Unit = {
    assertEquals((List(1), Nil),
      (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()))

    assertEquals((Nil, List("foo")),
      (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()))
  }

  @Test def tapLeft(): Unit = {
    assertEquals(List(1), ints().run(is ⇒      left(1).tapLeft(is += _)))
    assertEquals(Nil,     ints().run(is ⇒ right("foo").tapLeft(is += _)))
  }

  private def left(i: Int): Int \/ String = -\/(i)
  private def right(s: String): Int \/ String = \/-(s)
}