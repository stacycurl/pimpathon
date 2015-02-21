package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
import pimpathon.builder._
import pimpathon.util._


class BuilderTest {
  @Test def +++=(): Unit =
    assertEquals(List(1, 2, 3, 4), (ints() +++= List(List(1, 2), List(3, 4))).result())

  @Test def on(): Unit = {
    val (ib, sb) = ints() rpair (_.on[String](_.toInt) ++= List("1", "2"))
    assertEquals((List(1, 2), List(1, 2)), (sb.result(), ib.result()))

    sb.clear()
    assertEquals((Nil, Nil), (sb.result(), ib.result()))
  }

  @Test def reset(): Unit = {
    assertEquals((Nil, Nil),     ints()  |> (ib ⇒ (ib.reset(), ib.result())))
    assertEquals((List(1), Nil), ints(1) |> (ib ⇒ (ib.reset(), ib.result())))
  }

  @Test def run(): Unit = {
    assertEquals(Nil,        ints().run())
    assertEquals(List(1),    ints().run(_ += 1))
    assertEquals(List(1, 2), ints().run(_ += 1, _ += 2))
  }
}