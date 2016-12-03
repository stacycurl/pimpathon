package pimpathon.java.util.concurrent

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

import pimpathon.any._


object threadFactory {
  implicit class ThreadFactoryPimps(val self: ThreadFactory) extends AnyVal {
    def naming(f: Int ⇒ String): ThreadFactory = NamingThreadFactory(self, f)
  }

  case class NamingThreadFactory(adapted: ThreadFactory, f: Int ⇒ String) extends ThreadFactory {
    def newThread(r: Runnable): Thread = adapted.newThread(r).tap(_.setName(f(count.getAndIncrement)))

    private val count = new AtomicInteger(0)
  }
}