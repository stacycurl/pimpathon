package pimpathon

import org.junit.Test
import scala.util.Success

import pimpathon.builder._
import pimpathon.classTag._
import pimpathon.option._
import pimpathon.util._


class OptionTest {
  @Test def tap(): Unit = on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tap(ss += "none", ss += _))).produces(List("none"), List("some"))

  @Test def tapNone(): Unit = on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tapNone(ss += "none"))).produces(List("none"), Nil)

  @Test def tapSome(): Unit = on(none[String], some("some"))
    .calling(o ⇒ strings().run(ss ⇒ o.tapSome(ss += _))).produces(Nil, List("some"))

  @Test def getOrThrow(): Unit = {
    Some("present").getOrThrow("missing")                === "present"
    Some("present").getOrThrow(new Exception("missing")) === "present"
    Some("present").getOrThrow(util.goBoom: Exception)   === "present"

    assertThrows[NoSuchElementException]("missing")(None.getOrThrow("missing"))
    assertThrows[RuntimeException]("missing")(None.getOrThrow(new RuntimeException("missing")))
  }

  @Test def toTry(): Unit = {
    none[String].toTry.failed.map(_.getClass.getName) === Success[String](className[NoSuchElementException])
    Some("foo").toTry                                 === Success[String]("foo")
  }

  @Test def invert(): Unit = on(none[Int], some(0)).calling(_.invert(1)).produces(some(1), none[Int])

  @Test def amass(): Unit = on(none[Int], some(1), some(2), some(3))
    .calling(_.amass(util.partial(2 → none, 3 → some("three")))).produces(none, none, none, some("three"))

  private def none[A]: Option[A]       = None
  private def some[A](a: A): Option[A] = Some(a)
}