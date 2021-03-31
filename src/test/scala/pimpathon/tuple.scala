package pimpathon

import scala.language.implicitConversions

import pimpathon.builder._
import pimpathon.tuple._


class TupleSpec extends PSpec {
  "tap"   in strings().run(ss ⇒ (1, "foo").tap(i ⇒ s ⇒ ss += (s + i))) ≡ List("foo1")
  "calc"  in ("123", "abc").calc(_ + _) ≡ "123abc"
  "calcC" in ("123", "abc").calcC(a ⇒ b ⇒ a + b) ≡ "123abc"

  "to" in {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    (123, 456.0).to[String] ≡ ("123", "456.0")
  }

  "tmap" in (2, "abc").tmap(_ * 3, _.reverse) ≡ (6, "cba")

  "map1" in (2, "abc").map1(_ * 3) ≡ (6, "abc")

  "map2" in (2, "abc").map2(_.reverse) ≡ (2, "cba")

  "addTo" in
    (ints(), strings()).tap(is ⇒ ss ⇒ (1, "foo").addTo(is, ss)).tmap(_.result(), _.result()) ≡ (List(1), List("foo"))

  "removeFrom" in
    (ints(1), strings("foo")).tap(is ⇒ ss ⇒ (1, "foo").removeFrom(is, ss)).tmap(_.toList, _.toList) ≡ (Nil, Nil)
}