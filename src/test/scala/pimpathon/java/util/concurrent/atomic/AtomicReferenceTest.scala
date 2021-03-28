package pimpathon.java.util.concurrent.atomic

import java.util.concurrent.atomic.AtomicReference
import org.junit.Test
import pimpathon.any._
import pimpathon.java.util.concurrent.atomic.atomicReference._
import pimpathon.util._

class AtomicReferenceTest {
  @Test def update(): Unit = 
    (new AtomicReference(123) |> (ref => (ref.update(_ * 2), ref.get()))) === (246, 246)
}
