package stacycurl.scala.pimpathon

object function {
  implicit class PredicateOps[A](val p: A => Boolean) extends AnyVal {
    def exists: List[A] => Boolean = (_.exists(p))
    def forall: List[A] => Boolean = (_.forall(p))
  }
}
