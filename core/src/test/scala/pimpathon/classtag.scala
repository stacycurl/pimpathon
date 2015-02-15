package pimpathon

import org.junit.Assert._
import org.junit.Test


class ClassTagTest {
  @Test def className(): Unit = assertEquals("pimpathon.ClassTagTest", classTag.className[ClassTagTest])

  @Test def simplecCassName(): Unit = assertEquals("ClassTagTest", classTag.simpleClassName[ClassTagTest])

  @Test def klassOf(): Unit = assertEquals(classOf[ClassTagTest], classTag.klassOf[ClassTagTest])
}