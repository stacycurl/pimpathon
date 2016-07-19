package pimpathon.frills

import org.junit.Test
import pimpathon.frills.function._
import pimpathon.util.{on, partial}

import scalaz.{\/-, -\/}


class PartialFunctionTest {
  @Test def disjunction(): Unit = on(-\/(1), \/-("two"), -\/(3), \/-("four"))
    .calling((partial(1 → 11) \/ partial("two" → 22)).lift).produces(Some(11), Some(22), None, None)
}