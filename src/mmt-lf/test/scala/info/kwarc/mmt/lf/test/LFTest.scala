package scala.info.kwarc.mmt.lf.test

import scala.info.kwarc.mmt.api.test.MMTTest

class LFTest extends MMTTest("MMT/examples")("info.kwarc.mmt.lf.Plugin") {
  behavior of "LF"
  it should "build urtheories without Errors" in hl("build MMT/urtheories mmt-omdoc")
}