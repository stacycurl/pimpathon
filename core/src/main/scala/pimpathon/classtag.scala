package pimpathon

import scala.reflect.ClassTag


object classTag {
  def className[A: ClassTag]: String = klassOf[A].getName
  def simpleClassName[A: ClassTag]: String = klassOf[A].getSimpleName
  def klassOf[A](implicit tag: ClassTag[A]): Class[A] = tag.runtimeClass.asInstanceOf[Class[A]]
}