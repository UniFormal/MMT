object MitMTest extends MagicTest("lmfdb", "mitm", "scscp") {
  def run : Unit = {
    // load the (default) configuration
    hl("mitm use")

    // load a non-default configuration
    // see mmt-api/resources/mitm/config.default.json for an example
    //hl("mitm use /path/to/config.json")

    // turn on scscp on localhost:26134
    hl("scscp on 26134")
  }
}
