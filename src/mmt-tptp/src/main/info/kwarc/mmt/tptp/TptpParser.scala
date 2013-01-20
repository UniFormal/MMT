package info.kwarc.mmt.tptp

import tptp._
import java.io._
import info.kwarc.mmt.tptp.TptpUtils._

/**
 * Parsing of TPTP files or TPTP formula parts into a Java data-structure.
 */
class TptpParser {

  var parser: tptp.TptpParser = null
  val outputManager = new SimpleTptpParserOutput()

  /**
   * Allows to the formulas in tptpFile one by one with {@link #parseNext}
   */
  def this(tptpFile: File) = {
    this()
    val in = new FileInputStream(tptpFile)
    val lexer = new TptpLexer(new DataInputStream(in))
    parser = new tptp.TptpParser(lexer)
  }

  /**
   * Allows parsing of a single TPTP formula part with {@link #parseString}
   */
  def this(tptpFormula: String) = {
    this()
    val in = new ByteArrayInputStream(tptpFormula.getBytes());
    val lexer = new TptpLexer(new DataInputStream(in))
    parser = new tptp.TptpParser(lexer)
  }

  /**
   * Parse next formula in the TPTP file specified when creating the parser
   */
  def parseNext = parser.topLevelItem(outputManager)
  
  /**
   * This can be used to parse a TPTP formula [sub-part] specified as a string when
   * creating the parser.
   */
  def parseString: Any = {
    try {
      return parser.term(outputManager)
    } catch {
      case _ => log("Parsing term failed")
    }
    try {
      return parser.plain_term(outputManager)
    } catch {
      case _ => log("Parsing plain_term failed")
    }
    try {
      return parser.defined_term(outputManager)
    } catch {
      case _ => log("Parsing defined_term failed")
    }
    try {
      return parser.system_term(outputManager)
    } catch {
      case _ => log("Parsing system_term failed")
    }
    try {
      return parser.fof_formula(outputManager)
    } catch {
      case _ => log("Parsing fof_formula failed")
    }
    try {
      return parser.unitary_formula(outputManager)
    } catch {
      case _ => log("Parsing unitary_formula failed")
    }
    try {
      return parser.quantified_formula(outputManager)
    } catch {
      case _ => log("Parsing quantified_formula failed")
    }
    try {
      return parser.literal(outputManager)
    } catch {
      case _ => log("Parsing literal failed")
    }
    try {
      return parser.tptp_literals(outputManager)
    } catch {
      case _ => log("Parsing tptp_literals failed")
    }
    try {
      return parser.tptp_literal(outputManager)
    } catch {
      case _ => log("Parsing tptp_literal failed")
    }
    try {
      return parser.general_term(outputManager)
    } catch {
      case _ => log("Parsing general_term failed")
    }
    try {
      return parser.general_data(outputManager)
    } catch {
      case _ => log("Parsing general_data failed")
    }
    try {
      return parser.general_list(outputManager)
    } catch {
      case _ => log("Parsing general_list failed")
    }
    try {
      return parser.info_item(outputManager)
    } catch {
      case _ => log("Parsing info_item failed")
    }
    try {
      return parser.formula_role(outputManager)
    } catch {
      case _ => log("Parsing formula_role failed")
    }
    try {
      return parser.annotations(outputManager)
    } catch {
      case _ => log("Parsing annotations failed")
    }
    try {
      return parser.tptp_input(outputManager)
    } catch {
      case _ => log("Parsing tptp_input failed")
    }
    try {
      return parser.topLevelItem(outputManager)
    } catch {
      case _ => log("Parsing topLevelItem failed")
    }
    null
  }
}
