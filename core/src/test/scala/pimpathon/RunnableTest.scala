package pimpathon

import org.junit.Assert._
import org.junit.Test
import pimpathon.builder._
import pimpathon.runnable._
import pimpathon.util._


class RunnableTest {
  @Test def create(): Unit = assertEquals(List(1), ints().run(is ⇒ run(runnable.create(is += 1))))

  @Test def fromThunk(): Unit = assertEquals(List(3), ints().run(is ⇒ run(() ⇒ is += 3)))

  private def run(runnable: Runnable) = runnable.run()
}