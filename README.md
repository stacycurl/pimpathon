Pimpathon
=========

[![Build Status](https://api.travis-ci.org/stacycurl/pimpathon.png?branch=master)](https://travis-ci.org/stacycurl/pimpathon)
[![Stories in Ready](https://badge.waffle.io/stacycurl/pimpathon.png?label=ready&title=Ready)](http://waffle.io/stacycurl/pimpathon)
[![Coverage Status](https://coveralls.io/repos/stacycurl/pimpathon/badge.png)](https://coveralls.io/r/stacycurl/pimpathon)
[![Gitter chat](https://badges.gitter.im/stacycurl/pimpathon.png)](https://gitter.im/stacycurl/pimpathon)
[![Codacy](https://www.codacy.com/project/badge/ed149591303b4f2bb1575d20b5394fa0)](https://www.codacy.com/public/stacycurl/pimpathon.git)
[![Bintray](https://api.bintray.com/packages/stacycurl/repo/pimpathon-core/images/download.svg) ](https://bintray.com/stacycurl/repo/pimpathon-core/_latestVersion)

**Pimpathon** is a library that extends Scala & Java classes with 'extension methods' via the [Pimp my Library][pimp-my-library] pattern.

### Using Pimpathon

Release artefacts are built using [Travis CI][ci] and automatically published to Bintray. To include the repositories in your SBT build you should add,

```scala
resolvers += "jcenter" at "http://jcenter.bintray.com"
// or
resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/"
// or (for releases [1.5.0][r1.5.0], [1.4.0][r1.4.0] [1.3.0][r1.3.0], [1.2.0][r1.2.0], [1.1.0][r1.1.0], [1.0.0][r1.0.0])
resolvers += Resolver.sonatypeRepo("releases")
```

Builds are available for Scala 2.11.7

### pimpathon-core

The pimps in core depend only on the core scala & java libraries. You can use it by including the following:

```scala
libraryDependencies += "com.github.stacycurl" %% "pimpathon-core" % "1.6.1"
```

### pimpathon-frills

The aim of frills is to pimp everything else. You can use it by including the following:

```scala
libraryDependencies += "com.github.stacycurl" %% "pimpathon-frills" % "1.6.1" intransitive()
```

'intransitive' means that frills won't force you to depend on everything that's pimped, you'll only get pimps for types in libraries you already depend on.

[Documentation][doc]

Release notes: [1.5.0][r1.5.0], [1.4.0][r1.4.0] [1.3.0][r1.3.0], [1.2.0][r1.2.0], [1.1.0][r1.1.0], [1.0.0][r1.0.0]

### Contributors

+ Corina Usher <corina.usher@gmail.com> [@coughlac](https://twitter.com/coughlac)
+ Howard Branch <purestgreen@gmail.com> [@purestgreen](https://twitter.com/purestgreen)
+ Julien Truffaut <truffaut.julien@gmail.com> [@julien-truffaut](https://twitter.com/julien-truffaut)
+ Raymond Barlow <rbarlow@raymanoz.com> [@raymanoz](https://twitter.com/raymanoz)
+ Sam Halliday <sam.halliday@gmail.com> [@fommil](https://twitter.com/fommil)
+ Stacy Curl <stacy.curl@gmail.com> [@stacycurl](https://twitter.com/stacycurl)

[ci]: https://travis-ci.org/stacycurl/pimpathon
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~pimpathon
[pimp-my-library]:http://www.artima.com/weblogs/viewpost.jsp?thread=179766
[r1.0.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.0.0.md
[r1.1.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.1.0.md
[r1.2.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.2.0.md
[r1.3.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.3.0.md
[r1.4.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.4.0.md
[r1.5.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.5.0.md
[r1.6.0]: https://github.com/stacycurl/pimpathon/blob/master/docs/ReleaseNotes-1.6.0.md
[doc]: https://rawgit.com/stacycurl/pimpathon/master/docs/index.html
[olddoc]: https://github.com/stacycurl/pimpathon/blob/master/docs/Documentation.md
