package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.builder._


class BuilderTest {
  @Test def +++=(): Unit =
    assertEquals(List(1, 2, 3, 4), (new M.ListBuffer[Int] +++= List(List(1, 2), List(3, 4))).result())
}