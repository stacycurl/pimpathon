package pimpathon

import scala.util.{Failure, Success}

import pimpathon.builder._
import pimpathon.either.EitherPimps
import pimpathon.function._
import pimpathon.tuple._


class EitherSpec extends PSpec {
  "leftOr"  in on(Left("left"), Right("right")).calling(_.leftOr(_ + " !")).produces("left", "right !")
  "rightOr" in on(Left("left"), Right("right")).calling(_.rightOr(_ + " !")).produces("left !", "right")

  "rescue"  in on(Right(123), Left("456")).calling(_.rescue(_.toInt)).produces(123, 456)
  "valueOr" in on(Right(123), Left("456")).calling(_.valueOr(_.toInt)).produces(123, 456)

  "rescuePF" in on(Right(123), Left("456"), Left("123"))
    .calling(_.rescue(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))

  "valueOrPF" in on(Right(123), Left("456"), Left("123"))
    .calling(_.valueOr(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))

  "bimap" in on(left(1), right("foo"))
    .calling(_.bimap(_.toString, _.length)).produces(Left[String, Int]("1"), Right[String, Int](3))

  "leftMap" in on(left(1), right("foo"))
    .calling(_.leftMap(_.toString)).produces(Left[String, String]("1"), Right[String, String]("foo"))

  "rightMap" in on(left(1), right("foo"))
    .calling(_.rightMap(_.length)).produces(Left[Int, Int](1), Right[Int, Int](3))

  "leftFlatMap" in on(Right(123), Left("456"), Left("123"))
    .calling(_.leftFlatMap(partial("123" → 123).toRight)).produces(Right(123), Left("456"), Right(123))

  "rightFlatMap" in on(Left(123), Right("456"), Right("123"))
    .calling(_.rightFlatMap(partial("123" → 123).toLeft)).produces(Left(123), Right("456"), Left(123))

  "tap" in {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()) ≡ (List(1), Nil)

    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()) ≡ (
      Nil, List("foo")
    )
  }

  "tapLeft" in {
    ints().run(is ⇒      left(1).tapLeft(is += _)) ≡ List(1)
    ints().run(is ⇒ right("foo").tapLeft(is += _)) ≡ Nil
  }

  "tapRight" in {
    strings().run(ss ⇒      left(1).tapRight(ss += _)) ≡ Nil
    strings().run(ss ⇒ right("foo").tapRight(ss += _)) ≡ List("foo")
  }

  "addTo" in {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).addTo(is, ss)).tmap(_.result(), _.result())      ≡ (List(1), Nil)
    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").addTo(is, ss)).tmap(_.result(), _.result()) ≡ (Nil, List("foo"))
  }

  "removeFrom" in {
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ left(1).removeFrom(is, ss)).tmap(_.toList, _.toList) ≡ (Nil, List("oo"))
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ right("oo").removeFrom(is, ss)).tmap(_.toList, _.toList) ≡ (List(1), Nil)
  }

  "getMessage" in
    on(Left(boom), Right("foo")).calling(_.getMessage).produces(Some(boom.getMessage), None)

  "toTry" in on(Left[Throwable, String](boom), Right[Throwable, String]("foo"))
    .calling(_.toTry).produces(Failure[String](boom), Success[String]("foo"))

  "toOption" in on(Left[Throwable, String](boom), Right[Throwable, String]("foo"))
    .calling(_.toOption).produces(None, Some("foo"))

  private def left(i: Int): Either[Int, String] = Left(i)
  private def right(s: String): Either[Int, String] = Right(s)
}