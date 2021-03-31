package pimpathon

import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.scalaz.std.boolean._
import _root_.scalaz.{-\/, \/-}


class BooleanSpec extends PSpec {
  "asInt"     in truthTableFor(_.asInt).produces(0, 1)
  "option"    in truthTableFor(_.option(123)).produces(None, Some(123))
  "cond"      in truthTableFor(_.cond(123, 456)).produces(456, 123)
  "implies"   in truthTableFor(_ implies _).produces(t, t, f, t)
  "nor"       in truthTableFor(_ nor _).produces(t, f, f, f)
  "nand"      in truthTableFor(_ nand _).produces(t, t, t, f)

  "tapFalse" in {
    strings().run(ss ⇒ false.tapFalse(ss += "false")) ≡ List("false")
    strings().run(ss ⇒ true.tapFalse(ss += "false")) ≡ Nil
  }

  "tapTrue" in {
    strings().run(ss ⇒ false.tapTrue(ss += "true")) ≡ Nil
    strings().run(ss ⇒ true.tapTrue(ss += "true")) ≡ List("true")
  }

  "either" - {
    "or" in truthTableFor(_.either(123).or("456")).produces(Left("456"), Right(123))
  }

  "disjunction" - {
    "or" in truthTableFor(_.disjunction(123).or("456")).produces(-\/("456"), \/-(123))
  }
  
  private def truthTableFor[A](fn: Boolean => A): util.on[Boolean]#calling[A] =
    util.on(false, true).calling(fn)
  
  private def truthTableFor[A](fn: (Boolean, Boolean) ⇒ A)(implicit d: DummyImplicit): util.on[(Boolean, Boolean)]#calling[A] =
    util.on((f,f), (f,t), (t,f), (t,t)).calling(fn.tupled)

  private lazy val (t, f) = (true, false)
}