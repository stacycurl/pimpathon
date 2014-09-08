package pimpathon.java.lang


object threadLocal {
  def create[A](initial: A): ThreadLocal[A] = new ThreadLocal[A] {
    override def initialValue: A = initial
  }
}
