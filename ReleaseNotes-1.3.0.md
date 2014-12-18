## Release Notes 1.3.0

### Breaking changes & bug fixes
+ Map[K, V].partitionKeysBy(PartialFunction[K, C]): (Map[K, V], Map[C, V]) -- tuple reversed so now 'errors' are on the left
+ Map[K, V].partitionValuesBy(PartialFunction[V, W]): (Map[K, V], Map[K, W])) -- tuple reversed so now 'errors' are on the left
+ Multimap[F, V, K].multiMap.head succeeds for empty values

### Additions
+ [A].tapPF(PartialFunction[A, Discarded]): A
+ [A].transform(PartialFunction[A, A]): A
+ [A].unfold(A => Option[(B, A)]): Stream[B]
+ Boolean.asInt: Int
+ Boolean.either(R).or(L): Either[L, R] -- as in scalaz
+ List[Either[L, R]].partitionEithers: (List[L], List[R])
+ List[A].partitionBy(PartialFunction[A, B]): (List[A], List[B])
+ List[A].countBy(A => B): MultiMap[List, Int, A]
+ List[A].duplicatesBy(A => B): List[A]
+ List[A].amass(PartialFunction[A, List[B]]): List[B]
+ List[A].sizeGT(Int): Boolean
+ Map[K, V].partitionEntriesBy(PartialFunction[(K, V), (C, W)]): (Map[K, V], Map[C, W])
+ Map[K, V].mapEntries(K => V => (C, W)): Map[C, W]
+ MultiMap[F, K, V].sequence: F[Map[K, V]]
+ MultiMap[F, K, V].headTailOption: Option[(Map[K, V], MultiMap[F, K, V])]
+ MultiMap[F, K, V].multiMap.mapEntries(K => F[V] => (C, F[W]): MultiMap[F, C, W]
+ MultiMap[F, K, V].multiMap.values: F[V]
+ Predicate[A].guard(A => B): PartialFunction[A, B]
+ (A => B => C).tupled: ((A, B)) => C
+ (A => B).guardWith(Predicate[A]): PartialFunction[A, B]
+ PartialFunction[A, B].partition(CC[A]): (CC[A], CC[B])
+ PartialFunction[A, B].isUndefinedAt(A): Boolean
+ PartialFunction[A, B].second[C]: PartialFunction[(C, A), (C, B)]
+ PartialFunction[A, B].first[C]: PartialFunction[(A, C), (B, C)]
+ PartialFunction[A, A].unify: A => A
+ GTL[K].asMap.withConstValue[V]: Map[K, V]
+ GTL[A].asMap.withEntries(A => (K, V)): Map[K, V]
+ GTL[(K, V)].toMultiMap[F[_]]: MultiMap[F, K, V]
+ Generalize argument of partitionByPF from List to GenTraversableLike
+ Generalize List.partitionEithers to GenTraversableLike
+ mutable.Builder[A, B] +++= TraversableOnce[TraversableOnce[A]]
