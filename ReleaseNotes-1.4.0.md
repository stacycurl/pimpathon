## Release Notes 1.4.0-Snapshot

### Breaking changes & bug fixes

### Additions
+ [A].passesOne(Predicate[A]*): Option[A]
+ [A].passesAll(Predicate[A]*): Option[A]
+ [A].failsOne(Predicate[A]*): Option[A]
+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ MultiMap[F, K, V].flatMapValues(V => F[W]): MultiMap[F, K, W]
+ function.and(Predicate[A]*): Predicate[A]
+ function.or(Predicate[A]*): Predicate[A]
+ callable.create(=> A): Callable[A]