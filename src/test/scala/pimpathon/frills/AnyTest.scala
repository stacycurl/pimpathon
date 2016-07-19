package pimpathon.frills

import org.junit.Test

import pimpathon.frills.any._
import pimpathon.util._
import scalaz.syntax.validation._


class AnyTest {
  @Test def ensure(): Unit    = on(1, 2).calling(_.ensure("odd")(isEven)).produces("odd".failure, 2.success)
  @Test def ensureNel(): Unit = on(1, 2).calling(_.ensureNel("odd")(isEven)).produces("odd".failureNel, 2.successNel)

  private val isEven = (i: Int) ⇒ i % 2 == 0
}