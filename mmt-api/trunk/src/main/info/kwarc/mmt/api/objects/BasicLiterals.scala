package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import uom._

// This file declares rules for 4 types of literals
// These should be moved to an OpenMath plugin, but they are used by the API, e.g., for metadata

/** OpenMath OMI - unlimited precision integers */
object OMI extends RealizedType with IntegerLiteral {
   init(OpenMath._path ? "OMI")
}

/** OpenMath OMF - IEEE double precision floats */
object OMF extends RealizedType {
   init(OpenMath._path ? "OMF")
   type univ = Double
   def fromString(s: String) = s.toDouble
   override def lex = Some(new parser.NumberLiteralLexer(true, false))
}

/** OpenMath OMSTR - strings */ 
object OMSTR extends RealizedType {
   init(OpenMath._path ? "OMSTR")
   type univ = String
   def fromString(s: String) = s
   override def lex = quotedLiteral("")
}

/** URI literals, concrete syntax is uri"..." */
object URILiteral extends RealizedType {
   init(OpenMath._path ? "URI")
   type univ = utils.URI
   def fromString(s: String) = utils.URI(s)
   override def lex = quotedLiteral("uri")
}