## Release Notes 1.4.0-Snapshot

### Breaking changes & bug fixes

### Additions
+ Array[A].copyTo(srcPos, Array[A], destPos, length): Array[A]
+ MultiMap[F, K, V].flatMapValues(V => F[W]): MultiMap[F, K, W]
+ callable.create(=> A): Callable[A]