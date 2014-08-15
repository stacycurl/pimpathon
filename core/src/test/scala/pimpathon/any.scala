package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.util._


class AnyTest {
  @Test def tap_update_withSideEffect {
    val ints = new ListBuffer[Int]

    1.tap(ints += _, ints += _)
    assertEquals(List(1, 1), ints.toList)

    2.update(ints += _)
    assertEquals(List(1, 1, 2), ints.toList)

    3.withSideEffect(ints += _)
    assertEquals(List(1, 1, 2, 3), ints.toList)
  }

  @Test def tapIf {
    assertEquals(List(1, 3), new ListBuffer[Int].tap(ints => {
      List(1, 2, 3).foreach(i => i.tapIf(_ % 2 != 0)(ints += _))
    }).toList)
  }

  @Test def tapUnless {
    assertEquals(List(2), new ListBuffer[Int].tap(ints => {
      List(1, 2, 3).foreach(i => i.tapUnless(_ % 2 != 0)(ints += _))
    }).toList)
  }

  @Test def partialMatch {
    assertEquals(Some("Matched"), 1 partialMatch { case 1 => "Matched" })
    assertEquals(None,            0 partialMatch { case 1 => "Matched" })
  }

  @Test def lpair {
    assertEquals((10, 1), 1.lpair(_ * 10))
  }

  @Test def rpair {
    assertEquals((1, 10), 1.rpair(_ * 10))
  }

  @Test def filterSelf {
    assertEquals(List(None, Some(2), None, Some(4)),
      List(1, 2, 3, 4).map(_.filterSelf(_ % 2 == 0)))
  }

  @Test def withFinally {
    assertEquals(List("body: input", "finally: input", "done"), new ListBuffer[String].tap(strings => {
      strings += "input".withFinally(s => strings += "finally: " + s)(s => {strings += "body: " + s; "done"})
    }).toList)
  }

  @Test def attempt {
    assertEquals(Success(2), 1.attempt(_ * 2))
    assertEquals(Failure(boom), 1.attempt(_ => throw boom))
  }
}

