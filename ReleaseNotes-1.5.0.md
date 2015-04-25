## Release Notes 1.5.0-Snapshot

### Breaking changes & bug fixes
+ Moved runnable from pimpathon.java.lang to pimpathon package

### Removals

### Additions
+ [A].ensure(=> E)(Predicate[A]): Validation[E, A]
+ [A].ensureNel(=> E)(Predicate[A]): ValidationNel[E, A]
+ [A: Numeric].bounded(A, A): A
+ Boolean.cond(=> A, => A): A
+ Boolean.implies(Boolean): Boolean
+ Boolean.nor(Boolean): Boolean
+ Boolean.nand(Boolean): Boolean
+ Option[E].toFailureNel(A): ValidationNel[E, A]
+ Either[Throwable, R].getMessage: Option[String]
+ Try[A].getMessage: Option[String]
+ List[A].onlyOrThrow(List[A] => Exception): A
+ List[A].onlyEither: Either[List[A], A]
+ List[A].toNel: Option[NonEmptyList[A]]
+ List[A].zipExact(List[B]): (List[(A, B)], Option[Either[List[A], List[B]]])
+ List[A].zipExactWith(List[B])((A, B) ⇒ C): (List[C], Option[Either[List[A], List[B]]])
+ GTL[A].apoFold(B)((B, A) ⇒ Either[C, B]): Either[C, B]
+ Set[A].notContains(A): Boolean
+ Map[K, V].collectKeys(PartialFunction[K, C]): Map[C, V]
+ Map[K, V].collectValues(PartialFunction[V, W]): Map[K, W]
+ Map[K, V].containsEntry(K, V): Boolean
+ Map[K, V].containsEntry((K, V)): Boolean
+ Predicate[A].cond(=> B, => B): A => B
+ (A => B).attempt: A => Try[B]
+ Callable[A].attempt: Callable[Try[A]]
+ Date.addDay(Int): Date
+ InputStream.toByteArray: Array[Byte]
+ File.canon: File
+ File.readString()(implicit Codec): String
+ File.source()(implicit Codec): BufferedSource  -- Added implicit Codec argument
+ File.readLines()(implicit Codec): List[String] -- Added implicit Codec argument
+ Throwable.stackTraceAsString: String