package pimpathon

import scala.language.implicitConversions

import org.junit.Test

import pimpathon.builder._
import pimpathon.tuple._
import pimpathon.util._


class TupleTest {
  @Test def tap(): Unit   = strings().run(ss ⇒ (1, "foo").tap(i ⇒ s ⇒ ss += (s + i))) === List("foo1")
  @Test def calc(): Unit  = ("123", "abc").calc(_ + _) === "123abc"
  @Test def calcC(): Unit = ("123", "abc").calcC(a ⇒ b ⇒ a + b) === "123abc"

  @Test def to(): Unit = {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    (123, 456.0).to[String] === ("123", "456.0")
  }

  @Test def tmap(): Unit = (2, "abc").tmap(_ * 3, _.reverse) === (6, "cba")

  @Test def map1(): Unit = (2, "abc").map1(_ * 3) === (6, "abc")

  @Test def addTo(): Unit =
    (ints(), strings()).tap(is ⇒ ss ⇒ (1, "foo").addTo(is, ss)).tmap(_.result(), _.result()) === (List(1), List("foo"))

  @Test def removeFrom(): Unit =
    (ints(1), strings("foo")).tap(is ⇒ ss ⇒ (1, "foo").removeFrom(is, ss)).tmap(_.toList, _.toList) === (Nil, Nil)
}