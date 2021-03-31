package pimpathon


class ClassTagSpec extends PSpec {
  "className"       in classTag.className[ClassTagSpec]       ≡ "pimpathon.ClassTagSpec"
  "simplecCassName" in classTag.simpleClassName[ClassTagSpec] ≡ "ClassTagSpec"
  "klassOf"         in classTag.klassOf[ClassTagSpec]         ≡ classOf[ClassTagSpec]
}