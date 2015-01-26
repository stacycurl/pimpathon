## Release Notes 1.4.0-Snapshot

### Breaking changes & bug fixes

### Removals
+ List[A].mapNonEmpty(List[A] => B): Option[B]
+ Map[K, V].mapNonEmpty(Map[K, V] => B): Option[B]

### Additions
+ [A].calcIf(Predicate[A])(A => B): Option[B]
+ [A].calcPF(PartialFunction[A, B]): Option[B]
+ [A].passes.one(Predicate[A]*): Option[A]
+ [A].passes.all(Predicate[A]*): Option[A]
+ [A].passes.none(Predicate[A]*): Option[A]
+ [A].passes.some(Predicate[A]*): Option[A]
+ [A].fails.one(Predicate[A]*): Option[A]
+ [A].fails.all(Predicate[A]*): Option[A]
+ [A].fails.none(Predicate[A]*): Option[A]
+ [A].fails.some(Predicate[A]*): Option[A]
+ List[A].calcIfNonEmpty(List[A] => B): Option[B]
+ List[A].duplicates: List[A]
+ List[A].mapIfNonEmpty(A => B): Option[List[B]]
+ List[A].unsnocC(=> B, List[A] => A => B): B
+ List[List[A]].cartesianProduct: List[List[A]]
+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ Map[K, V].calcIfNonEmpty(Map[K, V] => B): Option[B]
+ MultiMap[F, K, V].flatMapValues(V => F[W]): MultiMap[F, K, W]
+ MultiMap[F, K, V].flatMapValuesU(V => G[W]): MultiMap[G, K, W]
+ MultiMap[F, K, V].multiMap.mapEntriesU(K => F[V] => (C, G[W]): MultiMap[G, C, W]
+ MultiMap[F, K, V].multiMap.sliding(Int): F[MultiMap[F, K, V]]
+ NestedMap[K1, K2, V].flipNesting: NestedMap[K2, K1, V]
+ NestedMap[K1, K2, V].nestedMap.mapValuesEagerly(V => W): NestedMap[K1, K2, W]
+ NestedMap[K1, K2, V].nestedMap.mapKeysEagerly(K2 => C): NestedMap[K1, C, W]
+ function.and(Predicate[A]*): Predicate[A]
+ function.or(Predicate[A]*): Predicate[A]
+ function.nand(Predicate[A]*): Predicate[A]
+ function.nor(Predicate[A]*): Predicate[A]
+ File.ancestors: Stream[File]
+ File.isAncestorOf(File): Boolean
+ InputStream.gunzip: GZIPInputStream
+ OutputStream.gzip: GZIPOutputStream
+ callable.create(=> A): Callable[A]
+ classTag.className[A]: String
+ classTag.simpleClassName[A]: String
+ classTag.klassOf[A]: String
