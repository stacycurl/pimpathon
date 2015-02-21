package pimpathon

import scala.language.implicitConversions

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.tuple._
import pimpathon.util._


class TupleTest {
  @Test def tap(): Unit = {
    assertEquals(List("foo1"), strings().run(ss ⇒ (1, "foo").tap(i ⇒ s ⇒ ss += (s + i))))
  }

  @Test def calc(): Unit = {
    assertEquals("123abc", ("123", "abc").calc(_ + _))
  }

  @Test def calcC(): Unit = {
    assertEquals("123abc", ("123", "abc").calcC(a ⇒ b ⇒ a + b))
  }

  @Test def to(): Unit = {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    assertEquals(("123", "456.0"), (123, 456.0).to[String])
  }

  @Test def tmap(): Unit = {
    assertEquals((6, "cba"), (2, "abc").tmap(_ * 3, _.reverse))
  }

  @Test def addTo(): Unit = assertEquals(
    (List(1), List("foo")), (ints(), strings()).tap(is ⇒ ss ⇒ (1, "foo").addTo(is, ss)).tmap(_.result(), _.result())
  )

  @Test def removeFrom(): Unit = assertEquals(
    (Nil, Nil), (ints(1), strings("foo")).tap(is ⇒ ss ⇒ (1, "foo").removeFrom(is, ss)).tmap(_.toList, _.toList)
  )
}