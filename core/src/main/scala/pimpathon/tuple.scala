package pimpathon


object tuple {
  implicit class Tuple2Ops[A, B](val t: (A, B)) extends AnyVal {
    def calc[C](f: (A, B) => C): C = f(t._1, t._2)
  }
}
