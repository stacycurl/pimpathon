package pimpathon

import pimpathon.dynamicVariable._

import scala.util.DynamicVariable


class DynamicVariableSpec extends PSpec {
  "modify" in {
    dyn.modify(_ * 2)
    dyn.value ≡ (123 * 2)
  }

  "withModification" in {
    dyn.withModification(_ * 2) {
      dyn.value ≡ (123 * 2)
      "foo"
    } ≡ "foo"

    dyn.value ≡ 123
  }

  private lazy val dyn: DynamicVariable[Int] = perTest.dynamicVariable(123)
}
