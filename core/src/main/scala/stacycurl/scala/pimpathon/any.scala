package stacycurl.scala.pimpathon

object any {
  implicit class AnyOps[A](a: A) {
    def tap(action: A => Unit): A = { action(a); a }
  }
}
