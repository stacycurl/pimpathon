package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import pimpathon.any._


class AnyTest {
  @Test def tap {
    val tapped = new ListBuffer[Int]

    1.tap(tapped += _)

    assertEquals(List(1), tapped.toList)
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
}

