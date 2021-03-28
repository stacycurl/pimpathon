package pimpathon

import org.junit.Test
import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.scalaz.std.boolean._
import pimpathon.util._
import _root_.scalaz.{-\/, \/-}


class BooleanTest {
  @Test def asInt(): Unit     = falseTrue(_.asInt).produces(0, 1)
  @Test def either_or(): Unit = falseTrue(_.either(123).or("456")).produces(Left("456"), Right(123))
  @Test def option(): Unit    = falseTrue(_.option(123)).produces(None, Some(123))
  @Test def cond(): Unit      = falseTrue(_.cond(123, 456)).produces(456, 123)
  @Test def implies(): Unit   = truthTableFor(_ implies _, t, t, f, t)
  @Test def nor(): Unit       = truthTableFor(_ nor _,     t, f, f, f)
  @Test def nand(): Unit      = truthTableFor(_ nand _,    t, t, t, f)

  @Test def tapFalse(): Unit = {
    strings().run(ss ⇒ false.tapFalse(ss += "false")) === List("false")
    strings().run(ss ⇒ true.tapFalse(ss += "false")) === Nil
  }

  @Test def tapTrue(): Unit = {
    strings().run(ss ⇒ false.tapTrue(ss += "true")) === Nil
    strings().run(ss ⇒ true.tapTrue(ss += "true")) === List("true")
  }

  @Test def disjunction_or(): Unit = falseTrue(_.disjunction(123).or("456")).produces(-\/("456"), \/-(123))

  private def truthTableFor(fn: (Boolean, Boolean) ⇒ Boolean, ff: Boolean, ft: Boolean, tf: Boolean, tt: Boolean): Unit =
    util.on((f,f), (f,t), (t,f), (t,t)).calling(fn.tupled).produces(ff, ft, tf, tt)

  private def falseTrue[A](f: Boolean ⇒ A): on[Boolean]#calling[A] = util.on(false, true).calling(f)
  private val (t, f) = (true, false)
}