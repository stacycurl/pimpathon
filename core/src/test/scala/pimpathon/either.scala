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
    List(Right(123), Left("456"), Left("123")).map(_.rescue(util.partial("123" -> 123)))
  )

  @Test def valueOrPF(): Unit = assertEquals(
    List(Right(123), Left("456"), Right(123)),
    List(Right(123), Left("456"), Left("123")).map(_.valueOr(util.partial("123" -> 123)))
  )

  @Test def bimap(): Unit = {
    assertEquals(Left[String, Int]("1"), Left[Int, String](1).bimap(_.toString, _.length))
    assertEquals(Right[String, Int](3), Right[Int, String]("foo").bimap(_.toString, _.length))
  }

  @Test def leftMap(): Unit = {
    assertEquals(Left[String, String]("1"), Left[Int, String](1).leftMap(_.toString))
    assertEquals(Right[String, String]("foo"), Right[Int, String]("foo").leftMap(_.toString))
  }

  @Test def rightMap(): Unit = {
    assertEquals(Left[String, Int]("1"), Left[String, String]("1").rightMap(_.length))
    assertEquals(Right[Int, Int](3), Right[Int, String]("foo").rightMap(_.length))
  }

  @Test def leftFlatMap(): Unit = assertEquals(
    List(Right(123), Left("456"), Right(123)),
    List(Right(123), Left("456"), Left("123")).map(_.leftFlatMap(partial("123" -> 123).toRight))
  )

  @Test def rightFlatMap(): Unit = assertEquals(
    List(Left(123), Right("456"), Left(123)),
    List(Left(123), Right("456"), Right("123")).map(_.rightFlatMap(partial("123" -> 123).toLeft))
  )

  @Test def tap(): Unit = {
    assertEquals((List(1), Nil),
      (ints, strings).tap(is => ss => Left[Int, String](1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()))

    assertEquals((Nil, List("foo")),
      (ints, strings).tap(is => ss => Right[Int, String]("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()))
  }

  @Test def rightBias(): Unit = {
    assertEquals(Right[String, Int](3).right, Right[String, Int](3): Either.RightProjection[String, Int])
    assertEquals(Right[String, Int](3), Right[String, Int](3).right: Either[String, Int])

    val result: Either[String, Int] = for {
      x <- Right[String, Int](3): Either[String, Int]
      y <- Right[String, Int](4): Either[String, Int]
    } yield x + y

    assertEquals(Right[String, Int](7), result)
  }
}