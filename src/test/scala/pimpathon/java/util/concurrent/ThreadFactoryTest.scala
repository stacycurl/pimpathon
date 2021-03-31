package pimpathon.java.util.concurrent

import java.util.concurrent.ThreadFactory
import pimpathon.PSpec
import pimpathon.any._
import pimpathon.java.util.concurrent.threadFactory._


class ThreadFactorySpec extends PSpec {
  "naming" in basic.naming("name:" + _).calc(factory ⇒ {
    factory.newThread(runnable).getName ≡ "name:0"
    factory.newThread(runnable).getName ≡ "name:1"
  })

  private lazy val basic    = new ThreadFactory { def newThread(r: Runnable): Thread = new Thread(r) }
  private lazy val runnable = pimpathon.runnable.create(())
}
