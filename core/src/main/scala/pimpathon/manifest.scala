package pimpathon


object manifest {
  def className[A: Manifest]: String = klassOf[A].getName
  def simpleClassName[A: Manifest]: String = klassOf[A].getSimpleName
  def klassOf[A](implicit manifest: Manifest[A]): Class[A] = manifest.erasure.asInstanceOf[Class[A]]
}
