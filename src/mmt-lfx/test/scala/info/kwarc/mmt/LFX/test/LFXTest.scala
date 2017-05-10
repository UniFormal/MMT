package info.kwarc.mmt.LFX.test

import scala.info.kwarc.mmt.api.test.MMTTest

class LFXTest extends MMTTest("MMT/LFX","MitM/Foundation","MitM/smglom")("info.kwarc.mmt.lf.Plugin","info.kwarc.mmt.LFX.Plugin") {
  behavior of "LF"
  //shouldhl("build MMT/urtheories scala-bin")
  shouldhl("build MMT/urtheories mmt-omdoc")
  //shouldhl("build MMT/LFX scala-bin")
  shouldhl("build MMT/LFX mmt-omdoc")
  //shouldhl("build MitM/Foundation scala-bin")
  shouldhl("build MitM/Foundation mmt-omdoc")
  //shouldhl("build MMT/examples scala-bin")
  shouldhl("log+ structure-checker")
  shouldhl("build MitM/smglom mmt-omdoc")
}