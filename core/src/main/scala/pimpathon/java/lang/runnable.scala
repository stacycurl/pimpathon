package pimpathon.java.lang


object runnable {
  def create(action: ⇒ Unit): Runnable = new Runnable {
    override def run(): Unit = action
  }
}