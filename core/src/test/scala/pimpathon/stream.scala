package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.stream._


class StreamTest {
  @Test def continuallyWhile {
    assertEquals(Stream.empty[Int], stream.continuallyWhile(1)(_ => false))
    assertEquals(List.fill(1000)(1), stream.continuallyWhile(1)(_ => true).take(1000).toList)

    val ints = M.Stack[Int](1, 2, 3)
    assertEquals(List(1, 2), stream.continuallyWhile(ints.pop())(_ < 3).toList)
  }
}

