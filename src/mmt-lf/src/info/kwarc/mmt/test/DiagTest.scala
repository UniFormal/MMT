package info.kwarc.mmt.test

import info.kwarc.mmt.test.testers._

object DiagTest extends MMTIntegrationTest()(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin")
) {
  def main(): Unit = {
    shouldCheck("MMT/mathscheme", "Example.mmt")()
  }
}
