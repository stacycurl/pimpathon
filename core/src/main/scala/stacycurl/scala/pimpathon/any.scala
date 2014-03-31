package stacycurl.scala.pimpathon

object any {
  implicit class AnyOps[A](a: A) {
    def tap(action: A => Unit): A = { action(a); a }

    def partialMatch[B](pf: PartialFunction[A, B]): Option[B] = PartialFunction.condOpt(a)(pf)
  }
}
