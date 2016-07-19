package pimpathon

import org.junit.Test
import pimpathon.builder._
import pimpathon.runnable._
import pimpathon.util._


class RunnableTest {
  @Test def create(): Unit    = ints().run(is ⇒ run(runnable.create(is += 1))) === List(1)
  @Test def fromThunk(): Unit = ints().run(is ⇒ run(() ⇒ is += 3)) === List(3)

  private def run(runnable: Runnable) = runnable.run()
}