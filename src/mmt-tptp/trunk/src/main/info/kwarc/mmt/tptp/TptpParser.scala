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

  def this(tptpFormula: String) = {
    this()
    val in = new ByteArrayInputStream(tptpFormula.getBytes());
    val lexer = new TptpLexer(new DataInputStream(in))
    parser = new tptp.TptpParser(lexer)
  }

  def parseNext = parser.topLevelItem(outputManager)
  
  def parseString: Any = {
    try {
      return parser.topLevelItem(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.tptp_input(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.term(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.plain_term(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.defined_term(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.system_term(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.fof_formula(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.unitary_formula(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.quantified_formula(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.literal(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.tptp_literals(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.tptp_literal(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.general_term(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.general_data(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.general_list(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.info_item(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.formula_role(outputManager)
    } catch {
      case _ => 
    }
    try {
      return parser.annotations(outputManager)
    } catch {
      case _ => 
    }
    null
  }
}
