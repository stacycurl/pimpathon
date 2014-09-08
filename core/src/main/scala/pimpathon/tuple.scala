package pimpathon


object tuple {
  implicit class Tuple2Ops[A, B](val t: (A, B)) extends AnyVal {
    def calc[C](f: (A, B) => C): C = f(t._1, t._2)
    def to[C](implicit ac: A => C, bc: B => C): (C, C) = (ac(t._1), bc(t._2))
    def tmap[C, D](f: A => C, g: B => D): (C, D) = (f(t._1), g(t._2))
  }
}
