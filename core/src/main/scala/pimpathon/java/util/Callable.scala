package pimpathon.java.util

import java.util.concurrent.Callable


object callable {
  def create[A](action: => A): Callable[A] = new Callable[A] {
    override def call(): A = action
  }
}