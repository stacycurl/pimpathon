package pimpathon

import org.junit.Test
import scala.util.{Failure, Success}

import org.junit.Assert._
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
    assertEquals((List(1), Nil),
      (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()))

    assertEquals((Nil, List("foo")),
      (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()))
  }

  @Test def tapLeft(): Unit = {
    assertEquals(List(1), ints().run(is ⇒      left(1).tapLeft(is += _)))
    assertEquals(Nil,     ints().run(is ⇒ right("foo").tapLeft(is += _)))
  }

  @Test def tapRight(): Unit = {
    assertEquals(Nil,         strings().run(ss ⇒      left(1).tapRight(ss += _)))
    assertEquals(List("foo"), strings().run(ss ⇒ right("foo").tapRight(ss += _)))
  }

  @Test def addTo(): Unit = {
    assertEquals((List(1), Nil),
      (ints(), strings()).tap(is ⇒ ss ⇒ left(1).addTo(is, ss)).tmap(_.result(), _.result()))

    assertEquals((Nil, List("foo")),
      (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").addTo(is, ss)).tmap(_.result(), _.result()))
  }

  @Test def removeFrom(): Unit = {
    assertEquals((Nil, List("foo")),
      (ints(1), strings("foo")).tap(is ⇒ ss ⇒ left(1).removeFrom(is, ss)).tmap(_.toList, _.toList))

    assertEquals((List(1), Nil),
      (ints(1), strings("foo")).tap(is ⇒ ss ⇒ right("foo").removeFrom(is, ss)).tmap(_.toList, _.toList))
  }

  @Test def toTry(): Unit = on(Right[Throwable, String]("foo"), Left[Throwable, String](boom))
    .calling(_.toTry).produces(Success[String]("foo"), Failure[String](boom))

  @Test def rightBias(): Unit = {
    assertEquals(right("foo").right, right("foo"): Either.RightProjection[Int, String])
    assertEquals(right("foo"), right("foo").right: Either[Int, String])

    val result: Either[Int, String] = for {
      x ← right("foo"): Either[Int, String]
      y ← right("oof"): Either[Int, String]
    } yield x + y

    assertEquals(right("foooof"), result)
  }

  private def left(i: Int): Either[Int, String] = Left(i)
  private def right(s: String): Either[Int, String] = Right(s)
}