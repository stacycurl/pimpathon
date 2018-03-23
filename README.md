Pimpathon
=========

[![Build Status](https://api.travis-ci.org/stacycurl/pimpathon.png?branch=master)](https://travis-ci.org/stacycurl/pimpathon)
[![Stories in Ready](https://badge.waffle.io/stacycurl/pimpathon.png?label=ready&title=Ready)](http://waffle.io/stacycurl/pimpathon)
[![Coverage Status](https://coveralls.io/repos/stacycurl/pimpathon/badge.png)](https://coveralls.io/r/stacycurl/pimpathon)
[![Gitter chat](https://badges.gitter.im/stacycurl/pimpathon.png)](https://gitter.im/stacycurl/pimpathon)
[![Bintray](https://api.bintray.com/packages/stacycurl/repo/pimpathon/images/download.svg) ](https://bintray.com/stacycurl/repo/pimpathon/_latestVersion)

**Pimpathon** is a library that extends Scala & Java classes with 'extension methods' via the [Pimp my Library][pimp-my-library] pattern.

Pimpathon contains pimps for classes in core scala & java libraries and pimps for external libraries. To avoid
name clash the pimps for core classes are called XPimps (ListPimps, etc.) and those for external libraries are called
XFrills (ListFrills, etc.)

### Using Pimpathon

Release artefacts are published to Bintray and are built using [Travis CI][ci].
The artefacts can be published to Sonatype upon request (if someone can tell me how to _fully_ automate that that would be great).

To include the repositories in your SBT build you should add:

```scala
resolvers += "jcenter" at "http://jcenter.bintray.com"
// or
resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/"
```

To include pimpathon as a dependency you should add:

```scala
libraryDependencies += "com.github.stacycurl" %% "pimpathon" % "1.8.16" intransitive()
```

'intransitive' means that even though pimpathon pimps a few third party libraries it won't force you to depend on them,
you'll only get pimps for types in libraries you already depend on.


Builds are available for Scala 2.11.7 & 2.12.0


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
