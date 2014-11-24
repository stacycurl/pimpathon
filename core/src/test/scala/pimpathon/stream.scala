package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.stream._


class StreamTest {
  @Test def cond(): Unit = {
    assertEquals(Stream.empty[Int], stream.cond(false, util.goBoom))
    assertEquals(Stream(1, 2, 3), stream.cond(true, Stream(1, 2, 3)))
  }

  @Test def continuallyWhile(): Unit = {
    assertEquals(Stream.empty[Int], stream.continuallyWhile(1)(_ => false))
    assertEquals(List.fill(1000)(1), stream.continuallyWhile(1)(_ => true).take(1000).toList)

    val ints = M.Stack[Int](1, 2, 3)
    assertEquals(List(1, 2), stream.continuallyWhile(ints.pop())(_ < 3).toList)
  }

  @Test def uncons(): Unit = {
    assertEquals("empty", Stream.empty[Int].uncons("empty", s => "size: " + s.size))
    assertEquals("size: 3", Stream(1, 2, 3).uncons("empty", s => "size: " + s.size))
  }

  @Test def tailOption(): Unit = {
    assertEquals(None,                     Stream.empty[Int].tailOption)
    assertEquals(Some(Stream.empty[Int]),  Stream(0).tailOption)
    assertEquals(Some(Stream(1)),          Stream(0, 1).tailOption)
  }
}

