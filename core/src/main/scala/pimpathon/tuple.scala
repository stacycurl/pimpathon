package pimpathon


object tuple {
  implicit def tuple2Ops[A, B](t: (A, B)): Tuple2Ops[A, B] = new Tuple2Ops[A, B](t)

  class Tuple2Ops[A, B](val t: (A, B)) {
    def calc[C](f: (A, B) => C): C = f(t._1, t._2)
    def to[C](implicit ac: A => C, bc: B => C): (C, C) = (ac(t._1), bc(t._2))
  }
}
