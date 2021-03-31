package pimpathon.frills

import pimpathon.PSpec
import pimpathon.frills.any._

import scalaz.syntax.validation._


class AnySpec extends PSpec {
  "ensure"    in on(1, 2).calling(_.ensure("odd")(isEven)).produces("odd".failure, 2.success)
  "ensureNel" in on(1, 2).calling(_.ensureNel("odd")(isEven)).produces("odd".failureNel, 2.successNel)

  private lazy val isEven = (i: Int) ⇒ i % 2 == 0
}