package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.either._
import pimpathon.function._
import pimpathon.tuple._
import pimpathon.util._


class EitherTest {
  @Test def leftOr(): Unit = assertEquals(
    List("left", "right !"),
    List(Left("left"), Right("right")).map(_.leftOr(_ + " !"))
  )

  @Test def rightOr(): Unit = assertEquals(
    List("left !", "right"),
    List(Left("left"), Right("right")).map(_.rightOr(_ + " !"))
  )

  @Test def rescue(): Unit = assertEquals(
    List(123, 456),
    List(Right(123), Left("456")).map(_ rescue(_.toInt))
  )

  @Test def valueOr(): Unit = assertEquals(
    List(123, 456),
    List(Right(123), Left("456")).map(_ valueOr(_.toInt))
  )

  @Test def rescuePF(): Unit = assertEquals(
    List(Right(123), Left("456"), Right(123)),
    List(Right(123), Left("456"), Left("123")).map(_.rescue(util.partial("123" → 123)))
  )

  @Test def valueOrPF(): Unit = assertEquals(
    List(Right(123), Left("456"), Right(123)),
    List(Right(123), Left("456"), Left("123")).map(_.valueOr(util.partial("123" → 123)))
  )

  @Test def bimap(): Unit = {
    assertEquals(Left[String, Int]("1"),     left(1).bimap(_.toString, _.length))
    assertEquals(Right[String, Int](3), right("foo").bimap(_.toString, _.length))
  }

  @Test def leftMap(): Unit = {
    assertEquals(Left[String, String]("1"),         left(1).leftMap(_.toString))
    assertEquals(Right[String, String]("foo"), right("foo").leftMap(_.toString))
  }

  @Test def rightMap(): Unit = {
    assertEquals(Left[Int, Int](1),       left(1).rightMap(_.length))
    assertEquals(Right[Int, Int](3), right("foo").rightMap(_.length))
  }

  @Test def leftFlatMap(): Unit = assertEquals(
    List(Right(123), Left("456"), Right(123)),
    List(Right(123), Left("456"), Left("123")).map(_.leftFlatMap(partial("123" → 123).toRight))
  )

  @Test def rightFlatMap(): Unit = assertEquals(
    List(Left(123), Right("456"), Left(123)),
    List(Left(123), Right("456"), Right("123")).map(_.rightFlatMap(partial("123" → 123).toLeft))
  )

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