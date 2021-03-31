package pimpathon

import pimpathon.any._
import pimpathon.builder._


class BuilderSpec extends PSpec {
  "+++=" in (ints() +++= List(List(1, 2), List(3, 4))).result() ≡ List(1, 2, 3, 4)

  "on" in {
    val (ib, sb) = ints() rpair (_.on[String](_.toInt) ++= List("1", "2"))
    (sb.result(), ib.result()) ≡ (List(1, 2), List(1, 2))

    sb.clear()
    (sb.result(), ib.result()) ≡ (Nil, Nil)
  }

  "reset" in {
    (ints()  |> (ib ⇒ (ib.reset(), ib.result()))) ≡ (Nil, Nil)
    (ints(1) |> (ib ⇒ (ib.reset(), ib.result()))) ≡ (List(1), Nil)
  }

  "run" in {
    ints().run() ≡ Nil
    ints().run(_ += 1) ≡ List(1)
    ints().run(_ += 1, _ += 2) ≡ List(1, 2)
  }
}