package scala.info.kwarc.mmt.lf.test

import scala.info.kwarc.mmt.api.test.MMTTest

class LFTest extends MMTTest("MMT/LFX","MitM/Foundation","Test/General")("info.kwarc.mmt.lf.Plugin") {
  behavior of "LF"
  // shouldhl("build MMT/urtheories scala-bin")
  shouldhl("build MMT/urtheories mmt-omdoc")
  //shouldhl("build MMT/examples scala-bin")
  //shouldhl("build MMT/examples mmt-omdoc")
  it should "fully build MMT/LFX: " in {
    // hl("build MMT/LFX scala-bin")
    hl("build MMT/LFX mmt-omdoc")
  }

  shouldhl("build Test/General mmt-omdoc hol.mmt")
  shouldhl("build Test/General mmt-omdoc views.mmt")
  shouldhl("build Test/General mmt-omdoc structures.mmt")
}