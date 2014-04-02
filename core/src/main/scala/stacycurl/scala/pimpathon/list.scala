package stacycurl.scala.pimpathon

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, ListBuffer, Map => MMap}


object list {
  implicit class ListOps[A](list: List[A]) {
    def emptyTo(alternative: => List[A]): List[A] = uncons(alternative, _ => list)

    def uncons[B](empty: => B, nonEmpty: List[A] => B): B = if (list.isEmpty) empty else nonEmpty(list)

    def toMapWithKeys[K](f: A => K): Map[K, A] = list.map(a => (f(a), a))(collection.breakOut)

    def toMapWithValues[V](f: A => V): Map[A, V] = list.map(a => (a, f(a)))(collection.breakOut)

    def toMapWithSomeKeys[K](f: A => Option[K]): Map[K, A] =
      list.flatMap(a => f(a).map(fa => (fa, a)))(collection.breakOut)

    def toMultiMapWithKeys[K](f: A => K): MultiMap[K, A] = list.map(a => (f(a), a))(MultiMap.build)
  }

  implicit class ListTuple2Ops[K, V](list: List[(K, V)]) {
    def toMultiMap: MultiMap[K, V] = list.map(kv => kv)(MultiMap.build)
  }
}

class MultiMapCanBuildFrom[From, K, V] extends CanBuildFrom[From, (K, V), MultiMap[K, V]] {
  def apply(from: From): Builder[(K, V), MultiMap[K, V]] = apply()
  def apply(): Builder[(K, V), MultiMap[K, V]] = new MultiMapBuilder[K, V]
}

class MultiMapBuilder[K, V](map: MMap[K, ListBuffer[V]] = MMap.empty[K, ListBuffer[V]])
  extends Builder[(K, V), MultiMap[K, V]] {

  def +=(elem: (K, V)): this.type = add(elem._1, elem._2)
  def clear(): Unit = map.clear()
  def result(): Map[K, List[V]] = map.map(kv => (kv._1, kv._2.toList))(collection.breakOut)

  def add(k: K, v: V): this.type = {
    map.update(k, map.get(k).fold(ListBuffer(v))(values => { values += v; values }))

    this
  }
}
