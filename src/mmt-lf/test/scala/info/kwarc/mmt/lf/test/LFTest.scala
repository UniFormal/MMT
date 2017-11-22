package scala.info.kwarc.mmt.lf.test

import scala.info.kwarc.mmt.api.test.MMTTest

class LFTest extends MMTTest("MMT/LFX","Test/General")("info.kwarc.mmt.lf.Plugin") {
  behavior of "LF"
  // shouldhl("build MMT/urtheories scala-bin")
  shouldhl("build MMT/urtheories mmt-omdoc")
  //shouldhl("build MMT/examples scala-bin")
  //shouldhl("build MMT/examples mmt-omdoc")
  shouldhl("build MMT/LFX mmt-omdoc")

  shouldcheck("Test/General","hol.mmt")(mayfail = List("http://test.kwarc.info/Structure?C?test2?definition"))
}