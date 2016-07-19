package pimpathon

import org.junit.Test
import pimpathon.util._


class ClassTagTest {
  @Test def className(): Unit       = classTag.className[ClassTagTest]       === "pimpathon.ClassTagTest"
  @Test def simplecCassName(): Unit = classTag.simpleClassName[ClassTagTest] === "ClassTagTest"
  @Test def klassOf(): Unit         = classTag.klassOf[ClassTagTest]         === classOf[ClassTagTest]
}