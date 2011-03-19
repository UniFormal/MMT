package info.kwarc.mmt.tptp

import tptp._
import java.io._

class TptpParser {

  var parser: tptp.TptpParser = null
  val outputManager = new SimpleTptpParserOutput()

  def this(tptpFile: File) = {
    this()
    val in = new FileInputStream(tptpFile)
    val lexer = new TptpLexer(new DataInputStream(in))
    parser = new tptp.TptpParser(lexer)
  }

  def parseNext: TptpParserOutput.TptpInput = parser.topLevelItem(outputManager)
}
