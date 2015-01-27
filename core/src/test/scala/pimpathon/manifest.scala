package pimpathon

import org.junit.Assert._
import org.junit.Test


class ManifestTest {
  @Test def className(): Unit = assertEquals("pimpathon.ManifestTest", manifest.className[ManifestTest])

  @Test def simplecCassName(): Unit = assertEquals("ManifestTest", manifest.simpleClassName[ManifestTest])

  @Test def klassOf(): Unit = assertEquals(classOf[ManifestTest], manifest.klassOf[ManifestTest])
}

