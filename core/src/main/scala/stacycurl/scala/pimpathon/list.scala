package stacycurl.scala.pimpathon

import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, ListBuffer, Map => MMap}


object list {
  implicit class ListOps[A](list: List[A]) {
    def emptyTo(alternative: => List[A]): List[A] = uncons(alternative, _ => list)

    def uncons[B](empty: => B, nonEmpty: List[A] => B): B = if (list.isEmpty) empty else nonEmpty(list)

    def toMapWithKeys[K](f: A => K): Map[K, A] = withKeys(f)(breakOut)

    def toMapWithValues[V](f: A => V): Map[A, V] = withValues(f)(breakOut)

    def toMapWithSomeKeys[K](f: A => Option[K]): Map[K, A] = withSomeKeys(f)(breakOut)

    def toMapWithSomeValues[V](f: A => Option[V]): Map[A, V] = withSomeValues(f)(breakOut)


    def toMultiMapWithKeys[K](f: A => K): MultiMap[K, A] = withKeys(f)(MultiMap.build)

    def toMultiMapWithSomeKeys[K](f: A => Option[K]): MultiMap[K, A] = withSomeKeys(f)(MultiMap.build)


    def withKeys[K, That](f: A => K)(implicit cbf: CanBuildFrom[List[A], (K, A), That]): That =
      list.map(a => (f(a), a))(cbf)

    def withValues[V, That](f: A => V)(implicit cbf: CanBuildFrom[List[A], (A, V), That]): That =
      list.map(a => (a, f(a)))(cbf)

    def withSomeKeys[K, That](f: A => Option[K])(implicit cbf: CanBuildFrom[List[A], (K, A), That]): That =
      list.flatMap(a => f(a).map(_ -> a))(cbf)

    def withSomeValues[V, That](f: A => Option[V])(implicit cbf: CanBuildFrom[List[A], (A, V), That]): That =
      list.flatMap(a => f(a).map(a -> _))(cbf)
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
  def result(): Map[K, List[V]] = map.map(kv => (kv._1, kv._2.toList))(breakOut)

  def add(k: K, v: V): this.type = {
    map.update(k, map.get(k).fold(ListBuffer(v))(values => { values += v; values }))

    this
  }
}
