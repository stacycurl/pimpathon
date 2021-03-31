package pimpathon.frills

import pimpathon.PSpec
import pimpathon.frills.function._
import scalaz.{-\/, \/, \/-}


class PartialFunctionSpec extends PSpec {
  "disjunction" in on[Int \/ String](-\/(1), \/-("two"), -\/(3), \/-("four"))
    .calling((partial(1 → 11) \/ partial("two" → 22)).lift).produces(Some(11), Some(22), None, None)
}