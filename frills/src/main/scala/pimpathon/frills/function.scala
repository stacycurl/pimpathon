package pimpathon.frills

import scala.language.higherKinds

import scala.{PartialFunction => ~>}
import scalaz.\/


object function {
  implicit class PartialFunctionFrills[In, Out](pf: In ~> Out) {
    def \/[In2](rhs: In2 ~> Out): (In \/ In2) ~> Out = new ((In \/ In2) ~> Out) {
      def isDefinedAt(in: In \/ In2): Boolean = in.fold(pf.isDefinedAt, rhs.isDefinedAt)
      def apply(in: In \/ In2): Out = in.fold(pf.apply, rhs.apply)
    }
  }
}