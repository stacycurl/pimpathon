package pimpathon.scalaz

import org.junit.Test
import scala.{PartialFunction ⇒ ~>}

import pimpathon.builder._
import pimpathon.tuple._
import pimpathon.util._
import pimpathon.scalaz.either._
import scalaz.syntax.std.option._

import scalaz.{\/-, -\/, \/}


class DisjunctionTest {
  @Test def tap(): Unit = {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()) === (List(1), Nil)

    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()) ===
      (Nil, List("foo"))
  }

  @Test def tapLeft(): Unit = {
    ints().run(is ⇒      left(1).tapLeft(is += _)) === List(1)
    ints().run(is ⇒ right("foo").tapLeft(is += _)) === Nil
  }

  @Test def tapRight(): Unit = {
    strings().run(ss ⇒      left(1).tapRight(ss += _)) === Nil
    strings().run(ss ⇒ right("foo").tapRight(ss += _)) === List("foo")
  }

  @Test def addTo(): Unit = {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).addTo(is, ss)).tmap(_.result(), _.result())      === (List(1), Nil)
    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").addTo(is, ss)).tmap(_.result(), _.result()) === (Nil, List("foo"))
  }

  @Test def removeFrom(): Unit = {
    (ints(1), strings("foo")).tap(is ⇒ ss ⇒ left(1).removeFrom(is, ss)).tmap(_.toList, _.toList) === (Nil, List("foo"))

    (ints(1), strings("foo")).tap(is ⇒ ss ⇒ right("foo").removeFrom(is, ss)).tmap(_.toList, _.toList) ===
      (List(1), Nil)
  }

  @Test def flatten(): Unit = {
    on(-\/(1), \/-(-\/(2)), \/-(\/-("s"))).calling(_.flatten).produces(-\/(1), -\/(2), \/-("s"))
    on(-\/(-\/(1)), -\/(\/-("s")), \/-("s")).calling(_.flatten).produces(-\/(1), \/-("s"), \/-("s"))
  }

  @Test def leftFlatMap(): Unit = on(\/-(123), -\/("456"), -\/("123"))
    .calling(_.leftFlatMap(partial("123" → 123).toRightDisjunction)).produces(\/-(123), -\/("456"), \/-(123))

  private def left(i: Int): Int \/ String = -\/(i)
  private def right(s: String): Int \/ String = \/-(s)

  implicit class PartialFunctionFrills[In, Out](pf: In ~> Out) {
    def toRightDisjunction: In ⇒ In \/ Out = (in: In) ⇒ pf.lift(in).toRightDisjunction(in)
  }
}