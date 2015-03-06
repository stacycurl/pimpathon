package pimpathon

import org.junit.Test
import scala.util.Success

import org.junit.Assert._
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
    assertEquals("present", Some("present").getOrThrow("missing"))
    assertEquals("present", Some("present").getOrThrow(new Exception("missing")))
    assertEquals("present", Some("present").getOrThrow(util.goBoom: Exception))

    assertThrows[NoSuchElementException]("missing")(None.getOrThrow("missing"))
    assertThrows[RuntimeException]("missing")(None.getOrThrow(new RuntimeException("missing")))
  }

  @Test def toTry(): Unit = {
    assertEquals(Success[String](className[NoSuchElementException]), none[String].toTry.failed.map(_.getClass.getName))
    assertEquals(Success[String]("foo"), Some("foo").toTry)
  }

  @Test def invert(): Unit = on(none[Int], some(0)).calling(_.invert(1)).produces(some(1), none[Int])

  private def none[A]: Option[A]       = None
  private def some[A](a: A): Option[A] = Some(a)
}