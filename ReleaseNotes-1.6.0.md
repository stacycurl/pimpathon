## Release Notes 1.6.0

### Breaking changes & bug fixes

### Removals

### Additions
+ List[A].onlyDisjunction: List[A] \/ A
+ (L \/ R).tap(L => Discarded, R => Discarded): L \/ R
+ (L \/ R).tapLeft(L => Discarded): L \/ R
+ argonaut.CodecJson[A].andThen(Json => Json): CodecJson[A]
+ argonaut.CodecJson[A].compose(Json => Json): CodecJson[A]
+ argonaut.DecodeJson[A].compose(Json => Json): DecodeJson[A]
+ argonaut.EncodeJson[A].andThen(Json => Json): EncodeJson[A]