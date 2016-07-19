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

Release artefacts are published to Bintray and are built using [Travis CI][ci].
The artefacts can be published to Sonatype upon request (if someone can tell me how to _fully_ automate that that would be great).

To include the repositories in your SBT build you should add,

```scala
resolvers += "jcenter" at "http://jcenter.bintray.com"
// or
resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/"
```

Builds are available for Scala 2.11.7

### pimpathon

Pimpathon contains pimps for classes in core scala & java libraries and pimps for external libraries. To avoid
name class the pimps for core classes are called XPimps (ListPimps, etc.) and those for external libraries are called
XFrills (ListFrills, etc.)

```scala
libraryDependencies += "com.github.stacycurl" %% "pimpathon" % "1.7.0" intransitive()
```

'intransitive' means that even though pimpathon pimps a few third party libraries it won't force you to depend on them,
you'll only get pimps for types in libraries you already depend on.

[Documentation][doc]

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
[doc]: https://rawgit.com/stacycurl/pimpathon/master/docs/index.html
[olddoc]: https://github.com/stacycurl/pimpathon/blob/master/docs/Documentation.md
