package scala.info.kwarc.mmt.lf.test

import scala.info.kwarc.mmt.api.test.MMTTest

class LFTest extends MMTTest("MMT/LFX","MitM/Foundation","MMT/examples")("info.kwarc.mmt.lf.Plugin") {
  behavior of "LF"
  //shouldhl("build MMT/urtheories scala-bin")
  shouldhl("build MMT/urtheories mmt-omdoc")
  //shouldhl("build MMT/LFX scala-bin")
  shouldhl("build MMT/LFX mmt-omdoc")
  //shouldhl("build MitM/Foundation scala-bin")
  shouldhl("build MitM/Foundation mmt-omdoc")
  //shouldhl("build MMT/examples scala-bin")
  shouldhl("build MMT/examples mmt-omdoc")
}