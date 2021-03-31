package pimpathon

import _root_.java.util.concurrent.atomic.AtomicReference
import org.scalatest.{BeforeAndAfterEach, FreeSpec}

import scala.util.DynamicVariable


trait PSpec extends FreeSpec with BeforeAndAfterEach with pimpathon.util {
  class Resetable[A](val get: A, resetIt: A => Unit) {
    def reset(): Unit = resetIt(get)
  }
  
  object perTest {
    def dynamicVariable[A](initial: => A): DynamicVariable[A] = {
      resetBeforeEach(new Resetable[DynamicVariable[A]](new DynamicVariable[A](initial), _.value = initial))
    }
  }
  
  protected def resetBeforeEach[A](result: Resetable[A]): A = {
    resetables.getAndUpdate((values: List[Resetable[_]]) => result :: values)

    result.get
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    
    resetables.get().foreach(_.reset())
  }

  private val resetables = new AtomicReference[List[Resetable[_]]](Nil)
}