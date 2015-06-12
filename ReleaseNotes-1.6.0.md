## Release Notes 1.6.0

### Breaking changes & bug fixes

### Removals

### Additions
+ List[A].onlyDisjunction: List[A] \/ A
+ GTL[A].asMap.withSomeEntries(A => Option[(K, V)]): Map[K, V]
+ GTL[A].asMultiMap.withSomeEntries(A => Option[(K, V)]): MultiMap[GTL, K, V]
+ (L \/ R).tap(L => Discarded, R => Discarded): L \/ R
+ (L \/ R).tapLeft(L => Discarded): L \/ R
+ (L \/ R).tapRight(R => Discarded): L \/ R
+ (L \/ R).addTo(Growable[L], Growable[R]): L \/ R
+ (L \/ R).removeFrom(Shrinkable[L], Shrinkable[R]): L \/ R
+ (PartialFunction[A, B] &&& PartialFunction[A, C]): PartialFunction[A, (B, C)]
+ argonaut.CodecJson[A].andThen(Json => Json): CodecJson[A]
+ argonaut.CodecJson[A].compose(Json => Json): CodecJson[A]
+ argonaut.DecodeJson[A].compose(Json => Json): DecodeJson[A]
+ argonaut.EncodeJson[A].andThen(Json => Json): EncodeJson[A]