Pimpathon
=========

![Build Status](https://github.com/stacycurl/pimpathon/actions/workflows/release.yml/badge.svg)
[![Release Artifacts][badge-release]][link-release]

**Pimpathon** is a library that extends Scala & Java classes with 'extension methods' via the [Pimp my Library][pimp-my-library] pattern.

Pimpathon contains pimps for classes in core scala & java libraries and pimps for external libraries. To avoid
name clash the pimps for core classes are called XPimps (ListPimps, etc.) and those for external libraries are called
XFrills (ListFrills, etc.)

### Using Pimpathon

Release artefacts are published to Sonatype and are built using Github Actions.

To include the repositories in your SBT build you should add:

```scala
resolvers += Resolver.sonatypeRepo("releases")
```

To include pimpathon as a dependency you should add:

```scala
libraryDependencies += "com.github.stacycurl" %% "pimpathon" % "1.8.27" intransitive()
```

'intransitive' means that even though pimpathon pimps a few third party libraries it won't force you to depend on them,
you'll only get pimps for types in libraries you already depend on.


Builds are available for Scala 2.11.7 & 2.12.12


[Documentation][doc]

### Contributors

+ Corina Usher <corina.usher@gmail.com> [@coughlac](https://twitter.com/coughlac)
+ Howard Branch <purestgreen@gmail.com> [@purestgreen](https://twitter.com/purestgreen)
+ Julien Truffaut <truffaut.julien@gmail.com> [@julien-truffaut](https://twitter.com/julien-truffaut)
+ Raymond Barlow <rbarlow@raymanoz.com> [@raymanoz](https://twitter.com/raymanoz)
+ Sam Halliday <sam.halliday@gmail.com> [@fommil](https://twitter.com/fommil)
+ Shing Hing Man <shinghing.man@gmail.com>
+ Stacy Curl <stacy.curl@gmail.com> [@stacycurl](https://twitter.com/stacycurl)
+ Xavier GUIHOT <x.guihot@gmail.com> (http://xavierguihot.com)

[pimp-my-library]:http://www.artima.com/weblogs/viewpost.jsp?thread=179766
[doc]: https://rawgit.com/stacycurl/pimpathon/master/docs/index.html
[olddoc]: https://github.com/stacycurl/pimpathon/blob/master/docs/Documentation.md
[link-release]: https://oss.sonatype.org/content/repositories/releases/com/github/stacycurl/pimpathon_2.12/ "Sonatype Releases"
[badge-release]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.stacycurl/pimpathon_2.12.svg "Sonatype Releases"

