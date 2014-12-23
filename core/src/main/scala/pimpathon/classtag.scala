package pimpathon

import scala.reflect.ClassTag


object classTag {
  def className[A](implicit tag: ClassTag[A]): String = tag.runtimeClass.getName
  def simpleClassName[A](implicit tag: ClassTag[A]): String = tag.runtimeClass.getSimpleName
}
