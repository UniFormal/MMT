package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import objects._
import symbols._
import documents._
import frontend._

/** a ParsingUnit represents a term that is to be parsed
 * @param source the source reference of the string to parse
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to parse
 * @param top an optional notation that the whole input must match; this can be used to parse a term that is known/required to have a certain form
 */
case class ParsingUnit(source: SourceRef, scope: Term, context: Context, term: String, top: Option[ParsingRule] = None)

class ParsingStream(val dpath: DPath, val stream: java.io.BufferedReader)

/**
 * an ObjectParser handles ParsingUnits
 *  
 * Instances are maintained by the ExtensionManager and retrieved and called by the structural parser.
 * 
 * see also [[Parser]]
 * 
 */
trait ObjectParser extends Extension {
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term
}

/** helper object */
object ObjectParser {
  val unknown = utils.mmt.mmtcd ? "unknown"
  def splitOffUnknowns(t: Term) = t match {
     case OMBIND(OMID(ObjectParser.unknown), us, s) => (us, s)
     case _ => (Context(), t)
  }
  /** @return true if t is a result of parsing that may need further analysis */
  def isOnlyParsed(t: Term) = t.head == Some(unknown)
}

//TODO: notations should not be computed separately for each ParsingUnit; they must be cached theory-wise

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultObjectParser extends ObjectParser {
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term = {
      val t = OMSemiFormal(objects.Text(pu.scope.toMPath.toPath, pu.term))
      SourceRef.update(t, pu.source)
      t
   }
}

/**
 * the type of structural parsers
 * 
 * see also [[Parser]]
 */
trait StructureParser extends Extension {
  /** the main interface function
    * 
    * @param r a Reader holding the input stream
    * @param dpath the MMT URI of the stream
    */
   def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) : Document
   
   /**
    * Reads from a string
    * @param text the string
    * @param dpath the id/location of the contained document
    */
   def readString(dpath: DPath, text: String)(implicit errorCont: ErrorHandler) = {
      val r = new java.io.BufferedReader(new java.io.StringReader(text))
      val ps = new ParsingStream(dpath, r)
      try {
         apply(ps)
      } finally {
         r.close
      }
   }
   /**
    * Reads from a file
    * @param text the string
    * @param dpath the id of the file
    */
   def readFile(dpath: DPath, f: utils.File)(implicit errorCont: ErrorHandler) = {
     val r = utils.File.Reader(f)
     val ps = new ParsingStream(dpath, r)
     try {
        apply(ps)
     } finally {
        r.close
     }
   }
}

/**
 * the designated super class for all parsers
 */
abstract class Parser(val objectLevel: ObjectParser) extends StructureParser with ObjectParser with LeveledExtension {
   /** relegates to objectParser */
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = objectLevel.apply(pu)
}