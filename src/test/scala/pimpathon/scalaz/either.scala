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
    locally {
      def l(i: Int): Int \/ (Int \/ String) = -\/(1)
      def m(i: Int): Int \/ (Int \/ String) = \/-(-\/(i))
      def r(s: String): Int \/ (Int \/ String) = \/-(\/-(s))
      
      on[Int \/ (Int \/ String)](l(1), m(2), r("s")).calling(_.flatten).produces(-\/(1), -\/(2), \/-("s"))
    }
    
    locally {
      def l(i: Int): (Int \/ String) \/ String = -\/(-\/(1))
      def m(s: String): (Int \/ String) \/ String = -\/(\/-(s))
      def r(s: String): (Int \/ String) \/ String = \/-(s)
      
      on[(Int \/ String) \/ String](l(1), m("s"), r("s")).calling(_.flatten).produces(-\/(1), \/-("s"), \/-("s"))
    }
  }

  @Test def leftFlatMap(): Unit = on[String \/ Int](\/-(123), -\/("456"), -\/("123"))
    .calling(_.leftFlatMap(partial("123" → 123).toRightDisjunction)).produces(\/-(123), -\/("456"), \/-(123))

  private def left(i: Int): Int \/ String = -\/(i)
  private def right(s: String): Int \/ String = \/-(s)

  implicit class PartialFunctionFrills[In, Out](private val self: In ~> Out) {
    def toRightDisjunction: In ⇒ In \/ Out = (in: In) ⇒ self.lift(in).toRightDisjunction(in)
  }
}