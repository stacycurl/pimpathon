package pimpathon.java.util.concurrent

import java.util.concurrent.ThreadFactory

import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
import pimpathon.java.util.concurrent.threadFactory._


class ThreadFactoryTest {
  @Test def naming(): Unit = basic.naming("name:" + _).calc(factory ⇒ {
    assertEquals("name:0", factory.newThread(runnable).getName)
    assertEquals("name:1", factory.newThread(runnable).getName)
  })

  private val basic    = new ThreadFactory { def newThread(r: Runnable): Thread = new Thread(r) }
  private val runnable = pimpathon.runnable.create(())
}
