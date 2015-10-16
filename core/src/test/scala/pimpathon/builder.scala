package pimpathon

import org.junit.Test

import pimpathon.any._
import pimpathon.builder._
import pimpathon.util._


class BuilderTest {
  @Test def +++=(): Unit = (ints() +++= List(List(1, 2), List(3, 4))).result() === List(1, 2, 3, 4)

  @Test def on(): Unit = {
    val (ib, sb) = ints() rpair (_.on[String](_.toInt) ++= List("1", "2"))
    (sb.result(), ib.result()) === (List(1, 2), List(1, 2))

    sb.clear()
    (sb.result(), ib.result()) === (Nil, Nil)
  }

  @Test def reset(): Unit = {
    (ints()  |> (ib ⇒ (ib.reset(), ib.result()))) === (Nil, Nil)
    (ints(1) |> (ib ⇒ (ib.reset(), ib.result()))) === (List(1), Nil)
  }

  @Test def run(): Unit = {
    ints().run() === Nil
    ints().run(_ += 1) === List(1)
    ints().run(_ += 1, _ += 2) === List(1, 2)
  }
}