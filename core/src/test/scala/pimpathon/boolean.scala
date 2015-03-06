package pimpathon

import org.junit.Test

import pimpathon.boolean._


class BooleanTest {
  @Test def asInt(): Unit     = calling(_.asInt).produces(1, 0)
  @Test def either_or(): Unit = calling(_.either(123).or("456")).produces(Right(123), Left("456"))
  @Test def option(): Unit    = calling(_.option(123)).produces(Some(123), None)

  private def calling[A](f: Boolean ⇒ A) = util.on(true, false).calling(f)
}