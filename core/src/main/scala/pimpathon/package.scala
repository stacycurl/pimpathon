import scala.collection.generic.CanBuildFrom


package object pimpathon {
  type CCBF[A, CC[_]] = CanBuildFrom[Nothing, A, CC[A]]
}
