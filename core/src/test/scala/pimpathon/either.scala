package pimpathon

import org.junit.Test
import scala.util.{Failure, Success}

import pimpathon.builder._
import pimpathon.either._
import pimpathon.function._
import pimpathon.tuple._
import pimpathon.util._


class EitherTest {
  @Test def leftOr(): Unit  = on(Left("left"), Right("right")).calling(_.leftOr(_ + " !")).produces("left", "right !")
  @Test def rightOr(): Unit = on(Left("left"), Right("right")).calling(_.rightOr(_ + " !")).produces("left !", "right")

  @Test def rescue(): Unit  = on(Right(123), Left("456")).calling(_.rescue(_.toInt)).produces(123, 456)
  @Test def valueOr(): Unit = on(Right(123), Left("456")).calling(_.valueOr(_.toInt)).produces(123, 456)

  @Test def rescuePF(): Unit = on(Right(123), Left("456"), Left("123"))
    .calling(_.rescue(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))

  @Test def valueOrPF(): Unit = on(Right(123), Left("456"), Left("123"))
    .calling(_.valueOr(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))

  @Test def bimap(): Unit = on(left(1), right("foo"))
    .calling(_.bimap(_.toString, _.length)).produces(Left[String, Int]("1"), Right[String, Int](3))

  @Test def leftMap(): Unit = on(left(1), right("foo"))
    .calling(_.leftMap(_.toString)).produces(Left[String, String]("1"), Right[String, String]("foo"))

  @Test def rightMap(): Unit = on(left(1), right("foo"))
    .calling(_.rightMap(_.length)).produces(Left[Int, Int](1), Right[Int, Int](3))

  @Test def leftFlatMap(): Unit = on(Right(123), Left("456"), Left("123"))
    .calling(_.leftFlatMap(partial("123" → 123).toRight)).produces(Right(123), Left("456"), Right(123))

  @Test def rightFlatMap(): Unit = on(Left(123), Right("456"), Right("123"))
    .calling(_.rightFlatMap(partial("123" → 123).toLeft)).produces(Left(123), Right("456"), Left(123))

  @Test def tap(): Unit = {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()) === (List(1), Nil)

    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()) === (
      Nil, List("foo")
    )
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
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ left(1).removeFrom(is, ss)).tmap(_.toList, _.toList) === (Nil, List("oo"))
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ right("oo").removeFrom(is, ss)).tmap(_.toList, _.toList) === (List(1), Nil)
  }

  @Test def getMessage(): Unit =
    on(Left(boom), Right("foo")).calling(_.getMessage).produces(Some(boom.getMessage), None)

  @Test def toTry(): Unit = on(Left[Throwable, String](boom), Right[Throwable, String]("foo"))
    .calling(_.toTry).produces(Failure[String](boom), Success[String]("foo"))

  @Test def rightBias(): Unit = {
    (right("foo"): Either.RightProjection[Int, String]) === right("foo").right
    (right("foo").right: Either[Int, String])           === right("foo")

    val result: Either[Int, String] = for {
      x ← right("foo"): Either[Int, String]
      y ← right("oof"): Either[Int, String]
    } yield x + y

    result === right("foooof")
  }

  private def left(i: Int): Either[Int, String] = Left(i)
  private def right(s: String): Either[Int, String] = Right(s)
}