package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.builder._


class BuilderTest {
  @Test def +++=(): Unit =
    assertEquals(List(1, 2, 3, 4), (new M.ListBuffer[Int] +++= List(List(1, 2), List(3, 4))).result())

  @Test def on(): Unit = {
    val (ib, sb) = new M.ListBuffer[Int] rpair (_.on[String](_.toInt) ++= List("1", "2"))
    assertEquals((List(1, 2), List(1, 2)), (sb.result(), ib.result()))

    sb.clear()
    assertEquals((Nil, Nil), (sb.result(), ib.result()))
  }

  @Test def reset(): Unit = {
    assertEquals((Nil, Nil),      new M.ListBuffer[Int]       |> (ib => (ib.reset(), ib.result())))
    assertEquals((List(1), Nil), (new M.ListBuffer[Int] += 1) |> (ib => (ib.reset(), ib.result())))
  }
}