package pimpathon

import scala.{PartialFunction => ~>}
import scala.collection.{GenTraversable, GenTraversableLike, GenTraversableOnce, breakOut, mutable => M}
import scala.collection.immutable.{SortedMap, TreeMap, Map => ▶:}
import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikePimpsMixin}
import pimpathon.any._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.tuple._


object map {
  implicit class MapPimps[K, V](self: K ▶: V)
    extends GenTraversableLikePimpsMixin[(K, V), GenTraversable]
    with GenTraversableLikeOfTuple2Mixin[K, V] {

    def containsAny(ok: Option[K]): Boolean = ok.exists(self.contains)
    def containsAll(ok: Option[K]): Boolean = ok.forall(self.contains)
    def containsAny[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.exists(self.contains)
    def containsAll[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.forall(self.contains)
    def containsEntry(kv: (K, V)): Boolean = containsEntry(kv._1, kv._2)
    def containsEntry(k: K, v: V): Boolean = self.get(k).contains(v)

    def get(ok: Option[K]): Option[V]               = ok.flatMap(self.get)
    def getOrThrow(k: K, message: String): V        = getOrThrow(k, new IllegalArgumentException(message))
    def getOrThrow(k: K, exception: ⇒ Exception): V = self.getOrElse(k, throw exception)

    def getOrLeft[L](k: K, l: => L): Either[L, V] = self.get(k).toRight(l)

    def findKey(p: Predicate[K]): Option[K]   = keyFor.matchingKey(p)
    def findValue(p: Predicate[V]): Option[V] = valueFor.matchingValue(p)

    def filterKeysNot(p: Predicate[K]):   K ▶: V = self.filterNot(kv ⇒ p(kv._1))
    def filterValuesNot(p: Predicate[V]): K ▶: V = self.filterNot(kv ⇒ p(kv._2))
    def filterValues(p: Predicate[V]):    K ▶: V = self.filter(kv ⇒ p(kv._2))


    def keyExists(p: Predicate[K]): Boolean   = self.exists(kv ⇒ p(kv._1))
    def valueExists(p: Predicate[V]): Boolean = self.exists(kv ⇒ p(kv._2))

    def emptyTo(empty: ⇒ K ▶: V): K ▶: V            = uncons(empty, _ ⇒ self)
    def calcIfNonEmpty[A](f: K ▶: V ⇒ A): Option[A]    = self.calcIf(_.nonEmpty)(f)
    def uncons[A](empty: ⇒ A, nonEmpty: K ▶: V ⇒ A): A = if (self.isEmpty) empty else nonEmpty(self)

    def reverse(f: Set[K] ⇒ K): V ▶: K = reverseToMultiMap.mapValuesEagerly(f)
    def reverseToMultiMap: V ▶: Set[K] = self.map(_.swap)(breakOut)

    def sortBy[C: Ordering](f: K => C): SortedMap[K, V] = sorted(Ordering[C].on[K](f))
    def sorted(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ self

    def composeM[C](other: C ▶: K): C ▶: V = other.andThenM(self)
    def andThenM[W](other: V ▶: W): K ▶: W = updateValues(other.get _)

    def toMutable: M.Map[K, V] = mutable
    def mutable: M.Map[K, V] = M.Map.empty[K, V] ++ self

    def entryFor: MapAndThen[K, V, (K, V)] = new MapAndThen[K, V, (K, V)](self, identity[(K, V)])
    def keyFor:   MapAndThen[K, V, K]      = new MapAndThen[K, V, K](self, key)
    def valueFor: MapAndThen[K, V, V]      = new MapAndThen[K, V, V](self, value)

    def partitionKeys(p: Predicate[K]):   (K ▶: V, K ▶: V) = self.partition(p.first[V])
    def partitionValues(p: Predicate[V]): (K ▶: V, K ▶: V) = self.partition(p.second[K])

    def partitionKeysBy[C](pf: K ~> C):   (K ▶: V, C ▶: V) = partitionEntriesBy(pf.first[V])
    def partitionValuesBy[W](pf: V ~> W): (K ▶: V, K ▶: W) = partitionEntriesBy(pf.second[K])

    def partitionEntriesBy[C, W](pf: (K, V) ~> (C, W)): (K ▶: V, C ▶: W) =
      self.partition(pf.isUndefinedAt).tmap(identity, _.map(pf))

    def mapKeysEagerly[C](f: K ⇒ C):         C ▶: V = self.map { case (k, v) ⇒ (f(k), v) }
    def mapValuesEagerly[W](f: V ⇒ W):       K ▶: W = self.map { case (k, v) ⇒ (k, f(v)) }
    def mapEntries[C, W](f: K ⇒ V ⇒ (C, W)): C ▶: W = self.map { case (k, v) ⇒ f(k)(v)   }

    def mapValuesWithKey[W](f: K => V => W): K ▶: W = self.map { case (k, v) => (k, f(k)(v)) }

    def seqMapKeys[C](f: K ⇒ Option[C]):                Option[C ▶: V] = seqMapEntries(k ⇒ v ⇒ f(k).map(_ → v))
    def seqMapValues[W](f: V ⇒ Option[W]):              Option[K ▶: W] = seqMapEntries(k ⇒ v ⇒ f(v).map(k → _))
    def seqMapEntries[C, W](f: K ⇒ V ⇒ Option[(C, W)]): Option[C ▶: W] = self.seqMap { case (k, v) ⇒ f(k)(v) }

    def collectKeys[C](pf: K ~> C):   C ▶: V = self.collect(pf.first)
    def collectValues[W](pf: V ~> W): K ▶: W = self.collect(pf.second)

    def updateValue(key: K, f: V ⇒ Option[V]): K ▶: V =
      self.get(key).flatMap(f).fold(self - key)(newValue ⇒ self + ((key, newValue)))

    def updateKeys[C](pf: K ~> C):   C ▶: V = updateKeys(pf.lift)
    def updateValues[W](pf: V ~> W): K ▶: W = updateValues(pf.lift)

    def updateKeys[C](f: K ⇒ Option[C]):   C ▶: V = self.flatMap(kv ⇒ f(kv._1).map(_ → kv._2))
    def updateValues[W](f: V ⇒ Option[W]): K ▶: W = self.flatMap(kv ⇒ f(kv._2).map(kv._1 → _))

    def zipWith[W, X](other: K ▶: W)(pf: (Option[V], Option[W]) ~> X): K ▶: X =
      self.keySet.union(other.keySet).flatMap(k ⇒ pf.lift((self.get(k), other.get(k))).map(k → _))(breakOut)

    protected def gtl: GenTraversableLike[(K, V), GenTraversable[(K, V)]] = self
    protected def cc: GenTraversable[(K, V)] = self
  }

  class MapAndThen[K, V, A](map: K ▶: V, andThen: ((K, V)) ⇒ A) {
    def maxValue(implicit O: Ordering[V]): Option[A] = map.calcIfNonEmpty(_.maxBy(value)).map(andThen)
    def minValue(implicit O: Ordering[V]): Option[A] = map.calcIfNonEmpty(_.minBy(value)).map(andThen)
    def maxKey(implicit O: Ordering[K]): Option[A] = map.calcIfNonEmpty(_.maxBy(key)).map(andThen)
    def minKey(implicit O: Ordering[K]): Option[A] = map.calcIfNonEmpty(_.minBy(key)).map(andThen)

    def matchingKey(p: Predicate[K]): Option[A]   = map.find(kv ⇒ p(kv._1)).map(andThen)
    def matchingValue(p: Predicate[V]): Option[A] = map.find(kv ⇒ p(kv._2)).map(andThen)
  }

  @inline private def key[K, V]:   ((K, V)) ⇒ K = (kv: (K, V)) ⇒ kv._1
  @inline private def value[K, V]: ((K, V)) ⇒ V = (kv: (K, V)) ⇒ kv._2
}