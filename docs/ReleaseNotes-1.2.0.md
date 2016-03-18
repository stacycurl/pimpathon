## Release Notes 1.2.0

### Breaking changes & bug fixes
+ InputStreamUtils.copy(InputStream, OutputStream, closeIn?, closeOut?): Unit -- no longer has an Array[Byte] buffer parameter, instead the InputStreamUtils constructor has a bufferSize parameter.

### Additions
+ [A].ifSelf(Predicate[A]): Option[A]
+ [A].unlessSelf(Predicate[A]): Option[A] -- (and an alias: filterNotSelf)
+ Either[L, R].rescue(PartialFunction[L, R]): Either[L, R] (alias for valueOr)
+ Either[L, R].rescue(L => R): R (alias for rightOr)
+ Either[L, R].valueOr(PartialFunction[L, R]): Either[L, R]
+ Either[L, R].valueOr(L => R): R (alias for rightOr)
+ Either[L, R].rightFlatMap(R => Either[L, R]): Either[L, R]
+ Either[L, R].leftFlatMap(L => Either[L, R]): Either[L, R]
+ Array[Byte].copyUpToN(Long, InputStream, OutputStream): Int
+ Array[Byte].readUpToN(Long, InputStream): Int
+ Stream[A].tailOption: Option[Stream[A]]
+ Stream[A].uncons(=> B, Stream[A] => B): B
+ Map[K, V].composeM(Map[C, K]): Map[C, V]
+ Map[K, V].keyExists(Predicate[K]): Boolean
+ Map[K, V].mapKeysEagerly(K => C): Map[C, V]
+ Map[K, V].partitionValuesBy(PartialFunction[V, W]): (Map[K, W], Map[K, V])) -- Changed in 1.3.0
+ Map[K, V].partitionKeysBy(PartialFunction[K, C]): (Map[C, V], Map[K, V])) -- Changed in 1.3.0
+ Map[K, V].updateKeys(PartialFunction[K, C]): Map[C, V]
+ Map[K, V].updateKeys(K => Option[C]): Map[C, V]
+ Map[K, V].updateValue(key: K, f: V => Option[V]): Map[K, V]
+ Map[K, V].updateValues(PartialFunction[V, W]): Map[K, W]
+ Map[K, V].updateValues(V => Option[W]]): Map[K, W]
+ MultiMap[F, K, V].multiMap.head: Map[K, V]
+ MultiMap[F, K, V].multiMap.tail: MultiMap[F, K, V]
+ MultiMap[F, K, V].multiMap.reverse: MultiMap[F, V, K]
+ MultiMap[F, K, V].pop(K): MultiMap[F, K, V]
+ PartialFunction[A, B].toLeft: A => Either[B, A]
+ PartialFunction[A, B].toRight: A => Either[A, B]
+ File.writeLines now appends a trailing newline
+ File.write(String, append?): File
+ Random.between(Int, Int): Int
+ OutputStream.writeN(InputStream, Long): OutputStream
+ OutputStream.writeUpToN(InputStream, Long): Long
+ InputStream.readN(OutputStream, Long): InputStream
+ InputStream.readUpToN(OutputStream, Long): Long

### Refactoring & Cleanup
+ Experimenting with expressing tests as single assertions
+ Refactored some InputStream & OutputStream methods
+ Refactoring test util.createInputStream
