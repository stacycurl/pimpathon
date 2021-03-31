package pimpathon

import pimpathon.builder._


class RunnableSpec extends PSpec {
  "create"    in ints().run(is ⇒ run(runnable.create(is += 1))) ≡ List(1)
  "fromThunk" in ints().run(is ⇒ run(() ⇒ is += 3)) ≡ List(3)

  private def run(runnable: Runnable): Unit = runnable.run()
}