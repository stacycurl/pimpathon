package pimpathon

import org.junit.Test

import pimpathon.boolean._


class BooleanTest {
  @Test def asInt(): Unit     = calling(_.asInt).produces(1, 0)
  @Test def either_or(): Unit = calling(_.either(123).or("456")).produces(Right(123), Left("456"))
  @Test def option(): Unit    = calling(_.option(123)).produces(Some(123), None)
  @Test def implies(): Unit   = truthTableFor(_ implies _, tt = true,  tf = false, ft = true,  ff = true)
  @Test def nor(): Unit       = truthTableFor(_ nor _,     tt = false, tf = false, ft = false, ff = true)
  @Test def nand(): Unit      = truthTableFor(_ nand _,    tt = false, tf = true,  ft = true,  ff = true)

  private def truthTableFor(f: (Boolean, Boolean) ⇒ Boolean, tt: Boolean, tf: Boolean, ft: Boolean, ff: Boolean) = {
    calling(f(true,  _)).produces(tt, tf)
    calling(f(false, _)).produces(ft, ff)
  }

  private def calling[A](f: Boolean ⇒ A) = util.on(true, false).calling(f)
}