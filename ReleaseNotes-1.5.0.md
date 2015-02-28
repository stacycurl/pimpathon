## Release Notes 1.5.0-Snapshot

### Breaking changes & bug fixes

### Removals

### Additions
+ GTL[A].apoFold(B)((B, A) ⇒ Either[C, B]): Either[C, B]
+ File.readString()(implicit Codec): String
+ File.source()(implicit Codec): BufferedSource  -- Added implicit Codec argument
+ File.readLines()(implicit Codec): List[String] -- Added implicit Codec argument