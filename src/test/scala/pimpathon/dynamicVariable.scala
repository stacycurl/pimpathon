package pimpathon

import org.junit.Test

import scala.util.DynamicVariable
import dynamicVariable._
import pimpathon.util._


class DynamicVariableTest {
  @Test def modify(): Unit = {
    dyn.modify(_ * 2)
    dyn.value === (123 * 2)
  }

  @Test def withModification(): Unit = {
    dyn.withModification(_ * 2) {
      dyn.value === (123 * 2)
      "foo"
    } === "foo"

    dyn.value === 123
  }

  private val dyn = new DynamicVariable[Int](123)
}
