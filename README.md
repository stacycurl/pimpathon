Pimpathon
=========

[![Build Status](https://api.travis-ci.org/stacycurl/pimpathon.png?branch=master)](https://travis-ci.org/stacycurl/pimpathon)
[![Stories in Ready](https://badge.waffle.io/stacycurl/pimpathon.png?label=ready&title=Ready)](http://waffle.io/stacycurl/pimpathon)
[![Coverage Status](https://coveralls.io/repos/stacycurl/pimpathon/badge.png)](https://coveralls.io/r/stacycurl/pimpathon)
[![Gitter chat](https://badges.gitter.im/stacycurl/pimpathon.png)](https://gitter.im/stacycurl/pimpathon)
[![Codacy](https://www.codacy.com/project/badge/ed149591303b4f2bb1575d20b5394fa0)](https://www.codacy.com/public/stacycurl/pimpathon.git)

**Pimpathon** is a library that extends Scala & Java classes with 'extension methods' via the [Pimp my Library][pimp-my-library] pattern.

### Using Pimpathon

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master and 2.9 branches are built using [Travis CI][ci] and automatically published
to the Sonatype OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
```

### pimpathon-core

Builds are available for Scala 2.9.2, 2.9.3, 2.10.4 & 2.11.2

```scala
libraryDependencies ++= Seq(
  "com.github.stacycurl" %% "pimpathon-core" % "1.3.0"
)
```

Release notes [1.3.0][r1.3.0], [1.2.0][r1.2.0], [1.1.0][r1.1.0], [1.0.0][r1.0.0]

### Contributors

+ Howard Branch <purestgreen@gmail.com> [@purestgreen](https://twitter.com/purestgreen)
+ Sam Halliday <sam.halliday@gmail.com> [@fommil](https://twitter.com/fommil)
+ Stacy Curl <stacy.curl@gmail.com> [@stacycurl](https://twitter.com/stacycurl)

[ci]: https://travis-ci.org/stacycurl/pimpathon
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~pimpathon
[pimp-my-library]:http://www.artima.com/weblogs/viewpost.jsp?thread=179766
[r1.0.0]: http://https://github.com/stacycurl/pimpathon/blob/master/ReleaseNotes-1.0.0.md
[r1.1.0]: http://https://github.com/stacycurl/pimpathon/blob/master/ReleaseNotes-1.1.0.md
[r1.2.0]: http://https://github.com/stacycurl/pimpathon/blob/master/ReleaseNotes-1.2.0.md
[r1.3.0]: http://https://github.com/stacycurl/pimpathon/blob/master/ReleaseNotes-1.3.0.md
[r1.4.0]: http://https://github.com/stacycurl/pimpathon/blob/master/ReleaseNotes-1.4.0.md
