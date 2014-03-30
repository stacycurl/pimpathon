package stacycurl.scala.pimpathon

import org.junit.Test
import scalaz.std.list._

import org.junit.Assert._
import stacycurl.scala.pimpathon.list._


class ListTest {
  @Test def uncons {
    assertEquals("empty", nil[Int].uncons("empty", l => s"size: ${l.size}"))
    assertEquals("size: 3", List(1, 2, 3).uncons("empty", l => s"size: ${l.size}"))
  }
}
