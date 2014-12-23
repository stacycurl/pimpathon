package pimpathon

import scala.reflect.ClassTag


object classTag {
  def className[A](implicit tag: ClassTag[A]): String = tag.runtimeClass.getName
}
