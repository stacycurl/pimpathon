package pimpathon


object threadLocal extends threadLocal

trait threadLocal {
  def create[A](initial: A): ThreadLocal[A] = new ThreadLocal[A] {
    override def initialValue: A = initial
  }
}