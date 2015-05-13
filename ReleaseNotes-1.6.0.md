## Release Notes 1.5.0-Snapshot

### Breaking changes & bug fixes

### Removals

### Additions
+ argonaut.CodecJson[A].andThen(Json => Json): CodecJson[A]
+ argonaut.CodecJson[A].compose(Json => Json): CodecJson[A]
+ argonaut.DecodeJson[A].compose(Json => Json): DecodeJson[A]
+ argonaut.EncodeJson[A].andThen(Json => Json): EncodeJson[A]