import scala.collection.generic.CanBuildFrom

import scala.language.higherKinds

package object pimpathon {
  type CCBF[A, CC[_]] = CanBuildFrom[Nothing, A, CC[A]]
}