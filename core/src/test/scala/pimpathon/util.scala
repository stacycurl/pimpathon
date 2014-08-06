package pimpathon

import scala.reflect.ClassTag


object util {
  def intercept[E <: AnyRef](f: => Any)(implicit expected: ClassTag[E]): E = {
    val clazz = expected.runtimeClass

    val caught = try { f; None } catch {
      case u: Throwable => if (clazz.isAssignableFrom(u.getClass)) Some(u) else {
        sys.error(s"Invalid exception, expected ${clazz.getName}, got: " + u)
      }
    }

    caught match {
      case None => sys.error(s"Expected exception: ${clazz.getName}")
      case Some(e) => e.asInstanceOf[E]
    }
  }
}
