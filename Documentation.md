## Core

The pimp in core depend only on the core scala & java libraries. You can use it by including the following:

```libraryDependencies += "com.github.stacycurl" %% "pimpathon-core" % "1.3.0"```

+ [A].calc(f: A => B): B (aka |>)
+ [A].calcIf(Predicate[A])(A => B): Option[B]
+ [A].calcUnless(Predicate[A])(A => B): Option[B]
+ [A].calcPF(PartialFunction[A, B]): Option[B]
+ [A].transform(PartialFunction[A, A]): A
+ [A].unfold(A => Option[(B, A)]): Stream[B]
+ [A].cond(Predicate[A], A => B, A => B): B
+ [A].attempt(A => B): Try[B] (Either[Throwable, B] for 2.9)
+ [A].attempt(A => B): Either[Throwable, B]
+ [A].lpair(A => B): (B, A)
+ [A].rpair(A => B): (A, B)
+ [A].partialMatch(PartialFunction[A, B]): Option[B]
+ [A].withFinally(A => Unit)(A => B): B
+ [A].addTo(Growable[A]): A
+ [A].filterSelf(Predicate[A]): Option[A]
+ [A].filterNotSelf(Predicate[A]): Option[A]
+ [A].ifSelf(Predicate[A]): Option[A]
+ [A].unlessSelf(Predicate[A]): Option[A] -- (and an alias: filterNotSelf)
+ [A].tap((A => Discarded)\*): A (aka update, aka withSideEffect)
+ [A].tapPF(PartialFunction[A, Discarded]): A
+ [A].tapIf(Predicate[A])((A => Unit)\*): A
+ [A].tapUnless(Predicate[A])((A => Unit)\*): A
+ [A].passes.one(Predicate[A]*): Option[A]
+ [A].passes.all(Predicate[A]*): Option[A]
+ [A].passes.none(Predicate[A]*): Option[A]
+ [A].passes.some(Predicate[A]*): Option[A]
+ [A].fails.one(Predicate[A]*): Option[A]
+ [A].fails.all(Predicate[A]*): Option[A]
+ [A].fails.none(Predicate[A]*): Option[A]
+ [A].fails.some(Predicate[A]*): Option[A]

+ (A, B).calc((A, B) => C): C
+ (A, B).calcC(A => B => C): C
+ (A, B).to[C]: (C, C)
+ (A, B).tmap(A => C, B => D): (C, D)
+ (A, B).tap((A => B => Discarded)*): (A, B)
+ (A, B).addTo(Growable[A], Growable[B]): (A, B)

+ Boolean.asInt: Int
+ Boolean.either(R).or(L): Either[L, R] -- as in scalaz

+ Option[A].getOrThrow(String): A
+ Option[A].getOrThrow(=> Exception): A
+ Option[A].invert(A): Option[A]
+ Option[A].toTry: Try[A]
+ Option[A].tap(none: => Unit, some: A => Unit): Option[A]
+ Option[A].tapNone(=> Unit): Option[A]
+ Option[A].tapSome(A => Unit): Option[A]

+ implicit conversion of Either to RightProjection (and back again)
+ Either[L, R].tap(L => Unit, R => Unit): Either[L, R]
+ Either[L, R].tapLeft(L => Unit): Either[L, R]
+ Either[L, R].tapRight(R => Unit): Either[L, R]
+ Either[L, R].addTo(Growable[L], Growable[R]): Either[L, R]
+ Either[L, R].rightOr(L => R): R (aka rescue, aka valueOr)
+ Either[L, R].leftOr(R => L): L
+ Either[L, R].bimap(L => M, R => S): Either[M, S]
+ Either[L, R].leftMap(L => M): Either[M, R]
+ Either[L, R].rightMap(R => S): Either[L, S]
+ Either[L, R].valueOr(PartialFunction[L, R]): Either[L, R] (aka rescue)
+ Either[L, R].rightFlatMap(R => Either[L, R]): Either[L, R]
+ Either[L, R].leftFlatMap(L => Either[L, R]): Either[L, R]
+ Either[Throwable, R].toTry: Try[R]

+ Try[A].toEither: Either[Throwable, A]

+ FilterMonadic[(K, V)].toMultiMap: MultiMap[List, K, V]

+ List[A].batchBy(A => B): List[List[A]]
+ List[A].countBy(A => B): MultiMap[List, Int, A]
+ List[A].distinctBy(A => B): List[A]
+ List[A].duplicatesBy(A => B): List[A]
+ List[A].duplicates: List[A]
+ List[A].const(B): List[B]
+ List[A].countWithSize(Predicate[A]): Option[(Int, Int)]
+ List[A].sizeGT(Int): Boolean
+ List[A].fraction(Predicate[A]): Double
+ List[A].emptyTo(List[A]): List[A]
+ List[A].headTail: (A, List[A])
+ List[A].headTailOption: Option[(A, List[A])]
+ List[A].tailOption: Option[List[A]]
+ List[A].initLast: (List[A], A)
+ List[A].initLastOption: Option[(List[A], A)]
+ List[A].prefixPadTo(Int, A): List[A]
+ List[A].sharedPrefix(List[A]): (List[A], List[A], List[A])
+ List[A].amass(PartialFunction[A, List[B]]): List[B]
+ List[A].calcIfNonEmpty(List[A] => B): Option[B]
+ List[A].mapIfNonEmpty(A => B): Option[List[B]]
+ List[A].onlyOption: Option[A]
+ List[A].uncons(=> B, List[A] => B): B
+ List[A].unconsC(=> B, A => List[A] => B): B
+ List[A].unsnocC(=> B, List[A] => A => B): B
+ List[A].zipWith(List[B])(((A, B)) => C): List[C]
+ List[K].zipToMap(List[V]): Map[K, V]
+ List[A].tapNonEmpty(List[A] => Unit): List[A]
+ List[A].tapEmpty(=> Unit): Lis[A]
+ List[A].tap(empty: => Unit, nonEmpty: List[A] => Unit): List[A]
+ List[List[A]].cartesianProduct: List[List[A]]


+ GTL[A].collectAttributeCounts(PartialFunction[A, B]): Map[B, Int]
+ GTL[A].optAttributeCounts(A => Option[B]): Map[B, Int]
+ GTL[A].attributeCounts(A => B): Map[B, Int]
+ GTL[V].asMap.withKeys(V => K): Map[K, V]
+ GTL[V].asMap.withSomeKeys(V => Option[K]): Map[K, V]
+ GTL[V].asMap.withManyKeys(V => List[K]): Map[K, V]
+ GTL[A].asMap.withUniqueKeys(A => K): Option[Map[K, A]]
+ GTL[V].asMap.withPFKeys(PartialFunction[V, K]): Map[K, V]
+ GTL[K].asMap.withValues(K => V): Map[K, V]
+ GTL[K].asMap.withSomeValues(K => Option[V]): Map[K, V]
+ GTL[K].asMap.withPFValues(PartialFunction[K, V]): Map[K, V]
+ GTL[K].asMap.withConstValue[V]: Map[K, V]
+ GTL[A].asMap.withEntries(A => (K, V)): Map[K, V]
+ GTL[V].asMultiMap.withKeys(V => K): MultiMap[GTL, K, V]
+ GTL[V].asMultiMap.withSomeKeys(V => Option[K]): MultiMap[GTL, K, V]
MultiMap[GTL, K, V]
+ GTL[V].asMultiMap.withManyKeys(V => List[K]): MultiMap[GTL, K, V]
+ GTL[A].asMultiMap.withUniqueKeys(A => K): Option[MultiMap[GTL, K, A]]
+ GTL[V].asMultiMap.withPFKeys(PartialFunction[V, K]): MultiMap[GTL, K, V]
+ GTL[K].asMultiMap.withValues(K => V): MultiMap[GTL, K, V]
+ GTL[K].asMultiMap.withSomeValues(K => Option[V]):
+ GTL[K].asMultiMap.withPFValues(PartialFunction[K, V]): Map[GTL, K, V]
+ GTL[K].asMultiMap.withConstValue[V]: MultiMap[GTL, K, V]
+ GTL[K].as[F[_. _]].with\*(K => V): F[K, V]
+ GTL[A].ungroupBy(A => B): GTL[GTL[A]]
+ GTL[A].partitionByPF(PartialFunction[A, B]): (GTL[A], GTL[B]) -- wrong ?
+ GTL[A].seqFold(B)((B, A) => Option[B]): Option[B]
+ GTL[A].seqMap[To](A => Option[B]): Option[To]
+ GTL[(K, V)].toMultiMap[F[_]]: MultiMap[F, K, V]
+ GTL[Either[L, R]].partitionEithers: (GTL[L], GTL[R])

+ Set[A].mutable: mutable.Set[A] (aka toMutable)
+ Set[A].powerSet: Set[Set[A]]

+ stream.cond(Boolean, => Stream[A]): Stream[A]
+ Stream.continuallyWhile(=> A)(Predicate[A]): Stream[A]
+ Stream[A].tailOption: Option[Stream[A]]
+ Stream[A].uncons(=> B, Stream[A] => B): B

+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ Array[Byte].toHex: String
+ Array[Byte].toHex(Int): String
+ Array[Byte].copyUpToN(Long, InputStream, OutputStream): Int
+ Array[Byte].readUpToN(Long, InputStream): Int


+ Map[K, V].getOrThrow(K, String): V
+ Map[K, V].getOrThrow(K, => Exception): V
+ Map[K, V].mapKeysEagerly(K => C): Map[C, V]
+ Map[K, V].mapValuesEagerly(V => W): Map[K, W]
+ Map[K, V].mapEntries(K => V => (C, W)): Map[C, W]
+ Map[K, V].updateKeys(PartialFunction[K, C]): Map[C, V]
+ Map[K, V].updateKeys(K => Option[C]): Map[C, V]
+ Map[K, V].updateValue(key: K, f: V => Option[V]): Map[K, V]
+ Map[K, V].updateValues(PartialFunction[V, W]): Map[K, W]
+ Map[K, V].updateValues(V => Option[W]]): Map[K, W]
+ Map[K, V].keyExists(Predicate[K]): Boolean
+ Map[K, V].valueExists(Predicate[V]): Boolean
+ Map[K, V].filterValues(Predicate[V]): Map[K, V]
+ Map[K, V].filterValuesNot(Predicate[V]): Map[K, V]
+ Map[K, V].filterKeysNot(Predicate[K]): Map[K, V]
+ Map[K, V].findValue(Predicate[V]): Option[V]
+ Map[K, V].findKey(Predicate[K]): Option[K]
+ Map[K, V].sorted(Ordering[K]): SortedMap[K, V]
+ Map[K, V].reverse(Set[K] => K): Map[V, K]
+ Map[K, V].reverseToMultiMap: MultiMap[Set, K, V]
+ Map[K, V].containsAll(Option[K]): Boolean
+ Map[K, V].containsAll(GTL[K]): Boolean
+ Map[K, V].containsAny(Option[K]): Boolean
+ Map[K, V].containsAny(GTL[K]): Boolean
+ Map[K, V].get(Option[K]): Option[V]
+ Map[K, V].emptyTo(Map[K, V]): Map[K, V]
+ Map[K, V].uncons(A, Map[K, V] => A): A
+ Map[K, V].entryFor.minKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryFor.maxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].entryFor.minKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryFor.maxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].entryFor.matchingValue(Predicate[V]): Option[(K, V)]
+ Map[K, V].entryFor.matchingKey(Predicate[K]): Option[(K, V)]
+ Map[K, V].keyFor.minValue(Ordering[V]): Option[K]
+ Map[K, V].keyFor.maxValue(Ordering[V]): Option[K]
+ Map[K, V].valueFor.minKey(Ordering[K]): Option[V]
+ Map[K, V].valueFor.maxKey(Ordering[K]): Option[V]
+ Map[K, V].andThenM(Map[V, W]): Map[K, W]
+ Map[K, V].composeM(Map[C, K]): Map[C, V]
+ Map[K, V].partitionKeysBy(PartialFunction[K, C]): (Map[K, V], Map[C, V])
+ Map[K, V].partitionValuesBy(PartialFunction[V, W]): (Map[K, V], Map[K, W]))
+ Map[K, V].partitionEntriesBy(PartialFunction[(K, V), (C, W)]): (Map[K, V], Map[C, W])
+ Map[K, V].calcIfNonEmpty(Map[K, V] => B): Option[B]
+ Map[K, V].mutable: mutable.Map[K, V] (aka toMutable)

+ MultiMap[F, K, V].select(F[V] => W): Map[K, W]
+ MultiMap[F, K, V].merge(MultiMap[F, K, V]): MultiMap[F, K, V]
+ MultiMap[F, K, V].append(K, F[V]): MultiMap[F, K, V]
+ MultiMap[F, K, V].multiMap.head: Map[K, V]
+ MultiMap[F, K, V].multiMap.tail: MultiMap[F, K, V]
+ MultiMap[F, K, V].multiMap.reverse: MultiMap[F, V, K]
+ MultiMap[F, K, V].pop(K): MultiMap[F, K, V]
+ MultiMap[F, K, V].sequence: F[Map[K, V]]
+ MultiMap[F, K, V].headTailOption: Option[(Map[K, V], MultiMap[F, K, V])]
+ MultiMap[F, K, V].multiMap.mapEntries(K => F[V] => (C, F[W]): MultiMap[F, C, W]
+ MultiMap[F, K, V].multiMap.values: F[V]
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

+ Predicate[A].ifSome: Predicate[Option[A]]
+ Predicate[A].and(Predicate[A]): Predicate[A]
+ Predicate[A].or(Predicate[A]): Predicate[A]
+ Predicate[A].exists: Predicate[List[A]]
+ Predicate[A].forall: Predicate[List[A]]
+ Predicate[A].guard(A => B): PartialFunction[A, B]
+ function.and(Predicate[A]*): Predicate[A]
+ function.or(Predicate[A]*): Predicate[A]
+ function.nand(Predicate[A]*): Predicate[A]
+ function.nor(Predicate[A]*): Predicate[A]

+ (A => B => C).tupled: ((A, B)) => C
+ (A => B).guardWith(Predicate[A]): PartialFunction[A, B]

+ PartialFunction[A, B].toLeft: A => Either[B, A]
+ PartialFunction[A, B].toRight: A => Either[A, B]
+ PartialFunction[A, B].either: A => Either[A, B]
+ PartialFunction[A, B].partition(CC[A]): (CC[A], CC[B])
+ PartialFunction[A, B].isUndefinedAt(A): Boolean
+ PartialFunction[A, B].second[C]: PartialFunction[(C, A), (C, B)]
+ PartialFunction[A, B].first[C]: PartialFunction[(A, C), (B, C)]
+ PartialFunction[A, A].unify: A => A
+ (PartialFunction[A, B] *** PartialFunction[C, D]): PartialFunction[(A, C), (B, D)]

+ Numeric[A].xmap(A => B, B => A): Numeric[B]

+ String.emptyTo(String): String
+ String.prefixPadTo(Int, Char): String
+ String.suffixWith(String): String
+ String.prefixWith(String): String
+ String.sharedPrefix(String): (String, String, String)
+ String.md5: String

+ Random.between(Int, Int): Int

+ threadLocal.create(A): ThreadLocal[A]

+ callable.create(=> A): Callable[A]

+ pimpathon.java.io forwarding package object

+ InputStream.closeAfter(InputStream => A): A
+ InputStream.attemptClose: Try[Unit]
+ InputStream.closeUnless(Boolean): InputStream
+ InputStream.closeIf(Boolean): InputStream
+ InputStream.drain(OutputStream, closeIn?, closeOut): InputStream
+ InputStream.>>(OutputStream): InputStream
+ InputStream.buffered: BufferedInputStream
+ InputStreamUtils.copy(InputStream, OutputStream, closeIn?, closeOut?): Unit -- no longer has an Array[Byte] buffer parameter, instead the InputStreamUtils constructor has a bufferSize parameter.
+ InputStream.readN(OutputStream, Long): InputStream
+ InputStream.readUpToN(OutputStream, Long): Long
+ InputStream.gunzip: GZIPInputStream

+ OutputStream.closeAfter(OutputStream => A): A
+ OutputStream.attemptClose: Try[Unit]
+ OutputStream.closeUnless(Boolean): OutputStream
+ OutputStream.closeIf(Boolean): OutputStream
+ OutputStream.drain(InputStream, closeOut?, closeIn?): OutputStream
+ OutputStream.<<(InputStream): OutputStream
+ OutputStream.buffered: BufferedOutputStream
+ OutputStream.writeN(InputStream, Long): OutputStream
+ OutputStream.writeUpToN(InputStream, Long): Long
+ OutputStream.gzip: GZIPOutputStream

+ File.isJar: Boolean
+ File.isClass: Boolean
+ File.isJava: Boolean
+ File.isScala: Boolean
+ File.missing: Boolean
+ File.isChildOf(File): Boolean
+ File.isParentOf(File): Boolean
+ File.isAncestorOf(File): Boolean
+ File.isContainedIn(File): Boolean
+ File.contains(File): Boolean
+ File.hasExtension(String): Boolean
+ (File / String): File
+ File.named(String): File
+ File.relativeTo(File): File
+ File.file(String, String): File
+ File.writeLines(List[String], append?): File
+ File.writeBytes(Array[Byte], append?): File
+ File.write(String, append?): File
+ File.deleteRecursivelyOnExit(): File
+ File.deleteRecursively(): File
+ File.changeToDirectory(): File
+ File.create(directory?): File
+ File.touch(): File
+ File.children: Stream[File]
+ File.childDirs: Stream[File]
+ File.tree: Stream[File]
+ File.ancestors: Stream[File]
+ File.path: List[String]
+ File.outputStream(append?): FileOutputStream
+ File.source(): BufferedSource
+ File.readBytes(): Array[Byte]
+ File.readLines(): List[String]
+ File.className(File): String
+ File.md5(): String
+ file.tempFile(String, String): File
+ file.tempDir(String, String): File
+ file.file(String): File
+ file.withTempDirectory(File => A): A
+ file.withTempDirectory(String, String)(File => A): A
+ file.withTempFile(File => A): A
+ file.withTempFile(String, String)(File => A): A
+ file.files(File, String\*): Stream[File]
+ file.cwd: File

+ mutable.Builder[A, B] +++= TraversableOnce[TraversableOnce[A]]
+ mutable.Builder[A, B].on(C => A): mutable.Builder[C, B]
+ mutable.Builder[A, B].reset(): B
+ mutable.Builder[A, B].run((M.Builder[A, B] => Discarded)*): B

+ mutable.Map[K, V].retainKeys(Predicate[K]): mutable.Map[K, V]
+ mutable.Map[K, V].retainValues(Predicate[V]): mutable.Map[K, V]

+ classTag.className[A]: String
+ classTag.simpleClassName[A]: String
+ classTag.klassOf[A]: String

## Frills

The aim of frills is to pimp everything else. You can use it (when it has been published) by including the following:

```libraryDependencies += "com.github.stacycurl" %% "pimpathon-frills" % "1.4.0" intransitive()```

'intransitive' means that frills won't force you to depend on everything that's pimped, you'll only get pimps for types in libraries you already depend on.

+ Option[A].toSuccessNel(E): ValidationNel[E, A]

+ argonaut.Json.filterNulls: Json

+ NonEmptyList[A].distinct: NonEmptyList[A]
+ NonEmptyList[A].distinctBy(A => B): NonEmptyList[A]
+ NonEmptyList[A: Order].max: A
+ NonEmptyList[A: Order].min: A
+ NonEmptyList[V].asMap.withKeys(V => K): Map[K, V]
+ NonEmptyList[V].asMultiMap[CC[_]].with*(V => K): MultiMap[CC, K, V]
+ NonEmptyList[K].as[F[_. _]].with*(K => V): F[K, V]
