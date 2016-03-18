## Release Notes 1.0.0

### Additions

+ [A].calc(f: A => B): B
+ [A].cond(Predicate[A], A => B, A => B): B
+ [A].attempt(A => B): Try[B] (Either[Throwable, B] for 2.9)
+ [A].filterSelf(Predicate[A]): Option[A]
+ [A].lpair(A => B): (B, A)
+ [A].rpair(A => B): (A, B)
+ [A].partialMatch(PartialFunction[A, B]): Option[B]
+ [A].tapIf(Predicate[A])((A => Unit)\*): A
+ [A].tapUnless(Predicate[A])((A => Unit)\*): A
+ [A].tap((A => Discarded)\*): A
+ [A].update(A => Discarded): A
+ [A].withSideEffect(A => Discarded): A
+ [A].withFinally(A => Unit)(A => B): B
+ (A, B).to[C]: (C, C)
+ (A, B).calc((A, B) => C): C
+ Option[A].getOrThrow(String): A
+ Option[A].getOrThrow(=> Exception): A
+ Option[A].toTry: Try[A]
+ Either[L, R].map(L => M, R => S): Either[M, S]
+ Either[L, R].tap(L => Unit, R => Unit): Either[L, R]
+ Either[L, R].rightOr(L => R): R
+ Either[L, R].leftOr(R => L): L
+ Either[Throwable, R].toTry: Try[R]
+ Try[A].toEither: Either[Throwable, A]
+ FilterMonadic[(K, V)].toMultiMap: MultiMap[List, K, V]
+ GTL[A].collectAttributeCounts(PartialFunction[A, B]): Map[B, Int]
+ GTL[A].optAttributeCounts(A => Option[B]): Map[B, Int]
+ GTL[A].attributeCounts(A => B): Map[B, Int]
+ GTL[V].asMap.withKeys(V => K): Map[K, V]
+ GTL[K].asMap.withValues(K => V): Map[K, V]
+ GTL[V].asMap.withSomeKeys(V => Option[K]): Map[K, V]
+ GTL[K].asMap.withSomeValues(K => Option[V]): Map[K, V]
+ GTL[V].asMap.withManyKeys(V => List[K]): Map[K, V]
+ GTL[V].asMap.withPFKeys(PartialFunction[V, K]): Map[K, V]
+ GTL[K].asMap.withPFValues(PartialFunction[K, V]): Map[K, V]
+ GTL[V].asMultiMap.withKeys(V => K): MultiMap[GTL, K, V]
+ GTL[K].asMultiMap.withValues(K => V): MultiMap[GTL, K, V]
+ GTL[V].asMultiMap.withSomeKeys(V => Option[K]): MultiMap[GTL, K, V]
+ GTL[K].asMultiMap.withSomeValues(K => Option[V]): MultiMap[GTL, K, V]
+ GTL[V].asMultiMap.withManyKeys(V => List[K]): MultiMap[GTL, K, V]
+ GTL[V].asMultiMap.withPFKeys(PartialFunction[V, K]): MultiMap[GTL, K, V]
+ GTL[K].asMultiMap.withPFValues(PartialFunction[K, V]): Map[GTL, K, V]
+ GTL[K].as[F[_. _]].with\*(K => V): F[K, V]
+ List[A].batchBy(A => B): List[List[A]]
+ List[A].const(B): List[B]
+ List[A].countWithSize(Predicate[A]): Option[(Int, Int)]
+ List[A].distinctBy(A => B): List[A]
+ List[A].emptyTo(List[A]): List[A]
+ List[A].fraction(Predicate[A]): Double
+ List[A].headTail: (A, List[A])
+ List[A].headTailOption: Option[(A, List[A])]
+ List[A].initLast: (List[A], A)
+ List[A].initLastOption: Option[(List[A], A)]
+ List[A].mapNonEmpty(List[A] => B): Option[B]
+ List[A].prefixPadTo(Int, A): List[A]
+ List[A].seqMap(A => Option[B]): Option[List[B]]
+ List[A].sharedPrefix(List[A]): (List[A], List[A], List[A])
+ List[A].tailOption: Option[List[A]]
+ List[A].uncons(B, List[A] => B): B
+ List[A].unconsC(=> B, A => List[A] => B): B
+ List[A].zipWith(List[B])(((A, B)) => C): List[C]
+ List[K].zipToMap(List[V]): Map[K, V]
+ Set[A].mutable: mutable.Set[A]
+ Set[A].toMutable: mutable.Set[A]
+ Set[A].powerSet: Set[Set[A]]
+ Stream.continuallyWhile(=> A)(Predicate[A]): Stream[A]
+ Array[Byte].toHex: String
+ Array[Byte].toHex(Int): String
+ Map[K, V].andThenM(Map[V, W]): Map[K, W]
+ Map[K, V].sorted(Ordering[K]): SortedMap[K, V]
+ Map[K, V].reverse(Set[K] => K): Map[V, K]
+ Map[K, V].reverseToMultiMap: MultiMap[Set, K, V]
+ Map[K, V].mutable: mutable.Map[K, V]
+ Map[K, V].toMutable: mutable.Map[K, V]
+ Map[K, V].entryForMinKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryForMaxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].entryForMinKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryForMaxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].containsAll(Option[K]): Boolean
+ Map[K, V].containsAll(GTL[K]): Boolean
+ Map[K, V].containsAny(Option[K]): Boolean
+ Map[K, V].containsAny(GTL[K]): Boolean
+ Map[K, V].get(Option[K]): Option[V]
+ Map[K, V].mapNonEmpty(Map[K, V] => A): Option[A]
+ Map[K, V].mapValuesEagerly(V => W): Map[K, W]
+ Map[K, V].keyForMinValue(Ordering[V]): Option[K]
+ Map[K, V].keyForMaxValue(Ordering[V]): Option[K]
+ Map[K, V].valueForMinKey(Ordering[K]): Option[V]
+ Map[K, V].valueForMaxKey(Ordering[K]): Option[V]
+ Map[K, V].emptyTo(Map[K, V]): Map[K, V]
+ Map[K, V].uncons(A, Map[K, V] => A): A
+ Map[K, V].getOrThrow(K, String): V
+ Map[K, V].getOrThrow(K, => Exception): V
+ Map[K, V].valueExists(Predicate[V]): Boolean
+ Map[K, V].filterValues(Predicate[V]): Map[K, V]
+ Map[K, V].filterValuesNot(Predicate[V]): Map[K, V]
+ Map[K, V].filterKeysNot(Predicate[K]): Map[K, V]
+ Map[K, V].findEntryWithValue(Predicate[V]): Option[(K, V)]
+ Map[K, V].findEntryWithKey(Predicate[K]): Option[(K, V)]
+ Map[K, V].findValue(Predicate[V]): Option[V]
+ Map[K, V].findKey(Predicate[K]): Option[K]
+ MultiMap[F, K, V].select(F[V] => W): Map[K, W]
+ MultiMap[F, K, V].merge(MultiMap[F, K, V]): MultiMap[F, K, V]
+ Predicate[A].ifSome: Predicate[Option[A]]
+ Predicate[A].and(Predicate[A]): Predicate[A]
+ Predicate[A].or(Predicate[A]): Predicate[A]
+ Predicate[A].exists: Predicate[List[A]]
+ Predicate[A].forall: Predicate[List[A]]
+ (PartialFunction[A, B] \*\*\* PartialFunction[C, D]): PartialFunction[(A, C), (B, D)]
+ Numeric[A].xmap(A => B, B => A): Numeric[B]
+ String.sharedPrefix(String): (String, String, String)
+ String.md5: String
+ String.prefixPadTo(Int, Char): String
+ String.suffixWith(String): String
+ String.prefixWith(String): String
+ threadLocal.create(A): ThreadLocal[A]
+ InputStream.closeAfter(InputStream => A): A
+ InputStream.attemptClose: Try[Unit]
+ InputStream.closeUnless(Boolean): InputStream
+ InputStream.closeIf(Boolean): InputStream
+ InputStream.drain(OutputStream, closeIn?, closeOut): InputStream
+ InputStream.>>(OutputStream): InputStream
+ OutputStream.closeAfter(OutputStream => A): A
+ OutputStream.attemptClose: Try[Unit]
+ OutputStream.closeUnless(Boolean): OutputStream
+ OutputStream.closeIf(Boolean): OutputStream
+ OutputStream.drain(InputStream, closeOut?, closeIn?): OutputStream
+ OutputStream.<<(InputStream): OutputStream
+ File.outputStream: FileOutputStream
+ File.outputStream(append?): FileOutputStream
+ File.deleteRecursivelyOnExit(): File
+ File.deleteRecursively(): File
+ File.relativeTo(File): File
+ File.md5(): String
+ File.writeLines(List[String], append?): File
+ File.writeBytes(Array[Byte], append?): File
+ File.readBytes(): Array[Byte]
+ File.readLines(): List[String]
+ (File / String): File
+ File.tree: Stream[File]
+ File.children: Stream[File]
+ File.path: List[String]
+ File.changeToDirectory(): File
+ File.named(String): File
+ File.create(directory?): File
+ File.source(): BufferedSource
+ file.tempFile(String, String): File
+ file.tempDir(String, String): File
+ file.file(String): File
+ file.withTempDirectory(File => A): A
+ file.withTempDirectory(String, String)(File => A): A
+ file.withTempFile(File => A): A
+ file.withTempFile(String, String)(File => A): A
+ file.files(File, String\*): Stream[File]
+ file.cwd: File
