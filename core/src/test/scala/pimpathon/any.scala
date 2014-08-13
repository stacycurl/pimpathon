package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import pimpathon.any._


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
    val ints = new ListBuffer[Int]

    List(1, 2, 3).foreach(i => i.tapIf(_ % 2 != 0)(ints += _))
    assertEquals(List(1, 3), ints.toList)
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
    val strings = new ListBuffer[String]

    strings += "input".withFinally(s => strings += "finally: " + s)(s => {strings += "body: " + s; "done"})

    assertEquals(List("body: input", "finally: input", "done"), strings.toList)
  }
}

