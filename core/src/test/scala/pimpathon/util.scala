package pimpathon

import scala.reflect.ClassManifest


object util {
  def intercept[E <: AnyRef](f: => Any)(implicit expected: ClassManifest[E]): E = {
    val clazz = expected.erasure

    val caught = try { f; None } catch {
      case u: Throwable => if (clazz.isAssignableFrom(u.getClass)) Some(u) else {
        sys.error("Invalid exception, expected %s, got: ".format(clazz.getName) + u)
      }
    }

    caught match {
      case None => sys.error("Expected exception: %s".format(clazz.getName))
      case Some(e) => e.asInstanceOf[E]
    }
  }
}
