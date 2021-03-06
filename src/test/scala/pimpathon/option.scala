package pimpathon

import org.junit.Test
import scala.util.Success

import pimpathon.builder._
import pimpathon.classTag._
import pimpathon.option._


class OptionSpec extends PSpec {
  "tap" in on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tap(ss += "none", ss += _))).produces(List("none"), List("some"))

  "tapNone" in on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tapNone(ss += "none"))).produces(List("none"), Nil)

  "tapSome" in on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tapSome(ss += _))).produces(Nil, List("some"))

  "getOrThrow" in {
    Some("present").getOrThrow("missing")                ≡ "present"
    Some("present").getOrThrow(new Exception("missing")) ≡ "present"
    Some("present").getOrThrow(util.goBoom: Exception)   ≡ "present"

    assertThrows[NoSuchElementException]("missing")(None.getOrThrow("missing"))
    assertThrows[RuntimeException]("missing")(None.getOrThrow(new RuntimeException("missing")))
  }

  "toTry" in {
    none[String].toTry.failed.map(_.getClass.getName) ≡ Success[String](className[NoSuchElementException])
    Some("foo").toTry                                 ≡ Success[String]("foo")
  }

  "invert" in on(none[Int], some(0)).calling(_.invert(1)).produces(some(1), none[Int])

  "amass" in on(none[Int], some(1), some(2), some(3))
    .calling(_.amass(util.partial(2 → none, 3 → some("three")))).produces(none, none, none, some("three"))

  "toEither" in{
    none[String].toEither(42, identity[String]) ≡ Left(42)
    some("forty two").toEither(42, identity[String]) ≡ Right("forty two")
    some("change me").toEither(1, s ⇒ s.dropRight(2) + "you") ≡ Right("change you")
  }

  "containedIn" in {
    on(some(1), none, some(2)).calling(_.containedIn(Set(0, 1))).produces(true, false, false)
    on(some(1), none, some(2)).calling(_.containedIn(0, 1)).produces(true, false, false)
  }

  private def none[A]: Option[A]       = None
  private def some[A](a: A): Option[A] = Some(a)
}