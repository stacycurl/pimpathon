## Release Notes 1.1.0

### Breaking changes & bug fixes
+ Remove support for null files
+ File.children & File.tree can now handle unreadable directories

### Removals

+ Either[L, R].map(L => M, R => S): Either[M, S]
+ Map[K, V].entryForMinKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryForMaxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].entryForMinKey(Ordering[K]): Option[(K, V)]
+ Map[K, V].entryForMaxValue(Ordering[V]): Option[(K, V)]
+ Map[K, V].findEntryWithValue(Predicate[V]): Option[(K, V)]
+ Map[K, V].findEntryWithKey(Predicate[K]): Option[(K, V)]
+ Map[K, V].keyForMinValue(Ordering[V]): Option[K]
+ Map[K, V].keyForMaxValue(Ordering[V]): Option[K]
+ Map[K, V].valueForMinKey(Ordering[K]): Option[V]
+ Map[K, V].valueForMaxKey(Ordering[K]): Option[V]


### Additions
+ [A].addTo(M.Builder[A, To]): A
+ [A].|>(A => B): B
+ [A].update now an alias for tap
+ [A].withSideEffect now an alias for tap
+ (A, B).tmap(A => C, B => D): (C, D)
+ implicit conversion of Either to RightProjection (and back again)
+ Either[L, R].bimap(L => M, R => S): Either[M, S]
+ Either[L, R].leftMap(L => M): Either[M, R]
+ Either[L, R].rightMap(R => S): Either[L, S]
+ List[A].tapNonEmpty(List[A] => Unit): List[A]
+ List[A].tapEmpty(=> Unit): Lis[A]
+ List[A].tap(empty: => Unit, nonEmpty: List[A] => Unit): List[A]
+ stream.cond(Boolean, => Stream[A]): Stream[A]
+ GTL[A].ungroupBy(A => B): GTL[GTL[A]]
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
+ MultiMap[F, K, V].append(K, F[V]): MultiMap[F, K, V]
+ PartialFunction[A, B].either: A => Either[A, B]
+ InputStream.buffered: BufferedInputStream
+ OutputStream.buffered: BufferedOutputStream
+ File.touch(): File
+ File.className(File): String
+ File.file(String, String): File
+ File.isContainedIn(File): Boolean
+ File.contains(File): Boolean
+ File.isChildOf(File): Boolean
+ File.isParentOf(File): Boolean
+ File.isJar: Boolean
+ File.isClass: Boolean
+ File.isJava: Boolean
+ File.isScala: Boolean
+ File.hasExtension(String): Boolean
+ File.childDirs: Stream[File]
+ File.missing: Boolean
