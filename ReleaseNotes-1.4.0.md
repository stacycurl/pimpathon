## Release Notes 1.4.0-Snapshot

### Breaking changes & bug fixes

### Additions
+ [A].passesOne(Predicate[A]*): Option[A]
+ [A].passesAll(Predicate[A]*): Option[A]
+ [A].failsOne(Predicate[A]*): Option[A]
+ [A].failsAll(Predicate[A]*): Option[A]
+ List[A].unsnocC(=> B, List[A] => A => B): B
+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ MultiMap[F, K, V].flatMapValues(V => F[W]): MultiMap[F, K, W]
+ MultiMap[F, K, V].flatMapValuesU(V => G[W]): MultiMap[G, K, W]
+ MultiMap[F, K, V].multiMap.sliding(Int): F[MultiMap[F, K, V]]
+ function.and(Predicate[A]*): Predicate[A]
+ function.or(Predicate[A]*): Predicate[A]
+ inputStream.gunzip: GZIPInputStream
+ outputStream.gzip: GZIPOutputStream
+ callable.create(=> A): Callable[A]
+ classTag.className[A]: String
+ classTag.simpleClassName[A]: String
+ classTag.klassOf[A]: String
