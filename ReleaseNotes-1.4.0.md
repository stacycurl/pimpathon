## Release Notes 1.4.0-Snapshot

### Breaking changes & bug fixes
+ File.write        overwrites by default (i.e. append = false, was true before)
+ File.writeLines   overwrites by default (i.e. append = false, was true before)
+ File.writeBytes   overwrites by default (i.e. append = false, was true before)
+ File.outputStream overwrites by default (i.e. append = false, was true before)

### Removals
+ List[A].mapNonEmpty(List[A] => B): Option[B]
+ Map[K, V].mapNonEmpty(Map[K, V] => B): Option[B]
+ File.outputStream: FileOutputStream

### Additions
+ frills sub-project
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
+ (A, B).calcC(A => B => C): C
+ (A, B).tap((A => B => Discarded)*): (A, B)
+ Option[A].toSuccessNel(E): ValidationNel[E, A]
+ Option[A].tap(none: => Unit, some: A => Unit): Option[A]
+ Option[A].tapNone(=> Unit): Option[A]
+ Option[A].tapSome(A => Unit): Option[A]
+ Option[A].invert(A): Option[A]
+ GTL[A].seqFold(B)((B, A) => Option[B]): Option[B]
+ GTL[A].seqMap[To](A => Option[B]): Option[To]
+ GTL[A].asMap.withUniqueKeys(A => K): Option[Map[K, A]]
+ GTL[A].asMultiMap[F[_]].withUniqueKeys(A => K): Option[MultiMap[F, K, A]]
+ List[A].calcIfNonEmpty(List[A] => B): Option[B]
+ List[A].duplicates: List[A]
+ List[A].mapIfNonEmpty(A => B): Option[List[B]]
+ List[A].unsnocC(=> B, List[A] => A => B): B
+ List[A].onlyOption: Option[A]
+ List[List[A]].cartesianProduct: List[List[A]]
+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ Map[K, V].calcIfNonEmpty(Map[K, V] => B): Option[B]
+ MultiMap[F, K, V].flatMapValues(V => F[W]): MultiMap[F, K, W]
+ MultiMap[F, K, V].flatMapValuesU(V => G[W]): MultiMap[G, K, W]
+ MultiMap[F, K, V].multiMap.mapEntriesU(K => F[V] => (C, G[W]): MultiMap[G, C, W]
+ MultiMap[F, K, V].multiMap.sliding(Int): F[MultiMap[F, K, V]]
+ MultiMap[F, K, V].getOrEmpty(K): F[V]
+ MultiMap[F, K, V].onlyOption: Option[Map[K, V]]
+ NestedMap[K1, K2, V].append(K1, K2, V): NestedMap[K1, K2, V]
+ NestedMap[K1, K2, V] + ((K1, K2, V)): NestedMap[K1, K2, V]
+ NestedMap[K1, K2, V].flipNesting: NestedMap[K2, K1, V]
+ NestedMap[K1, K2, V].getOrEmpty(K1): Map[K2, V]
+ NestedMap[K1, K2, V].nestedMap.mapValuesEagerly(V => W): NestedMap[K1, K2, W]
+ NestedMap[K1, K2, V].nestedMap.mapKeysEagerly(K2 => C): NestedMap[K1, C, W]
+ function.and(Predicate[A]*): Predicate[A]
+ function.or(Predicate[A]*): Predicate[A]
+ function.nand(Predicate[A]*): Predicate[A]
+ function.nor(Predicate[A]*): Predicate[A]
+ String.emptyTo(String): String
+ FileUtils append constructor parameter (specifies the default value of append method parameter)
+ File.ancestors: Stream[File]
+ File.isAncestorOf(File): Boolean
+ pimpathon.java.io forwarding package object
+ InputStream.gunzip: GZIPInputStream
+ OutputStream.gzip: GZIPOutputStream
+ callable.create(=> A): Callable[A]
+ classTag.className[A]: String
+ classTag.simpleClassName[A]: String
+ classTag.klassOf[A]: String
+ mutable.Builder[A, B].on(C => A): mutable.Builder[C, B]
+ mutable.Builder[A, B].reset(): B
+ mutable.Builder[A, B].run((M.Builder[A, B] => Discarded)*): B
+ mutable.Map[K, V].retainKeys(Predicate[K]): mutable.Map[K, V]
+ mutable.Map[K, V].retainValues(Predicate[V]): mutable.Map[K, V]
+ argonaut.Json.filterNulls: Json
+ scalaz.NonEmptyList[A].distinct: NonEmptyList[A]
+ NonEmptyList[V].asMap.withKeys(V => K): Map[K, V]
+ NonEmptyList[V].asMultiMap[CC[_]].with*(V => K): MultiMap[CC, K, V]
+ NonEmptyList[K].as[F[_. _]].with*(K => V): F[K, V]
+ NonEmptyList[A].distinctBy(A => B): NonEmptyList[A]
+ NonEmptyList[A: Order].max: A
+ NonEmptyList[A: Order].min: A
