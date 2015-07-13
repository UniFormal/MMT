package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

/**
 * ParsingUnit encapsulates the input of an [[ObjectParser]]
 * @param source the source reference of the string to parse
 * @param context the context against which to parse
 * @param term the term to parse
 * @param top an optional notation that the whole input must match; this can be used to parse a term that is known/required to have a certain form
 */
case class ParsingUnit(source: SourceRef, context: Context, term: String, val nsMap: NamespaceMap, top: Option[ParsingRule] = None)

/**
 * ParsingStream encapsulates the input of a [[StructureParser]]
 * @param source the URI of the stream
 * @param dpath the URI of the document in the stream
 * @param nsMap defined namespaces
 * @param format the format of the stream
 * @param stream the stream to parse
 */
case class ParsingStream(source: URI, dpath: DPath, nsMap: NamespaceMap, format: String, stream: java.io.BufferedReader)

object ParsingStream {
   /** to allow passing a string instead of a reader */
   implicit def stringToReader(s: String) = new java.io.BufferedReader(new java.io.StringReader(s))
   /** to allow passing a file instead of a reader */
   implicit def fileToReader(f: File) = File.Reader(f)

   /**
    * creates a ParsingStream from a file, making reasonable default choices
    * @param f the file
    * @param dpathOpt the logical URI of the file (defaults to file URI with "omdoc" ending)
    * @param nsMapOpt the namespace map
    * @param formatOpt format (defaults to file extension)
    * @param streamOpt stream (defaults to reader from file)
    */
   def fromFile(f: File, dpathOpt: Option[DPath] = None, nsMapOpt: Option[NamespaceMap] = None,
                formatOpt: Option[String] = None, streamOpt: Option[java.io.BufferedReader] = None) = {
     val dpath = dpathOpt.getOrElse(DPath(FileURI(f.setExtension("omdoc"))))
     val nsMap = nsMapOpt.getOrElse(NamespaceMap(dpath))
     val format = formatOpt.getOrElse(f.getExtension.getOrElse(""))
     val stream = streamOpt.getOrElse(fileToReader(f))
     new ParsingStream(FileURI(f), dpath, nsMap, format, stream)
   }

   /**
    * creates a ParsingStream from a string
    * @param s the string
    * @param dpath the logical URI of the string
    * @param format format
    * @param nsMapOpt the namespace map
    */
   def fromString(s: String, dpath: DPath, format: String, nsMapOpt: Option[NamespaceMap] = None) = {
     val nsMap = nsMapOpt.getOrElse(NamespaceMap(dpath))
     new ParsingStream(dpath.uri, dpath, nsMap, format, s)
   }

   /**
    * creates a parsing stream for a source file in an archive
    *
    * @param a an archive
    * @param inPath path to the source file
    * @param strOpt the reader to use (defaults to file reader)
    *
    * @return a parsing stream where
    * source: logical document URI with native file extension
    * dpath: logical document URI with "omdoc" file extension
    * format: file extension
    */
   def fromSourceFile(a: Archive, inPath: FilePath, strOpt: Option[java.io.BufferedReader] = None) = {
     val inPathOMDoc = inPath.toFile.setExtension("omdoc").segments
      val base = a.narrationBase
      val dpath = DPath(base / inPathOMDoc) // bf.narrationDPath except for extension
      val stream = strOpt.getOrElse(File.Reader(a / source / inPath))
      ParsingStream(base / inPath.segments, dpath, a.namespaceMap(dpath), inPath.toFile.getExtension.getOrElse(""), stream)
   }
}

/**
 * an ObjectParser handles ParsingUnits
 *
 * Instances are maintained by the ExtensionManager and retrieved and called by the structural parser.
 *
 * see also [[Parser]]
 *
 */
trait ObjectParser extends FormatBasedExtension {
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
   def isApplicable(format: String) = true
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term = {
      val t = OMSemiFormal(objects.Text("unparsed", pu.term))
      SourceRef.update(t, pu.source)
      t
   }
}

/**
 * the type of structural parsers
 *
 * see also [[Parser]]
 */
trait StructureParser extends FormatBasedExtension {
 /** the main interface function: parses a stream and registers all elements (usually a single document) in it
    *
    * @param r a Reader holding the input stream
    * @param dpath the MMT URI of the stream
    * @return the document into which the stream was parsed
    */
   def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) : Document

}

/**
 * the designated super class for all parsers
 */
abstract class Parser(val objectLevel: ObjectParser) extends StructureParser with ObjectParser with LeveledExtension {
   def format: String
   def isApplicable(s: String) = s == format
   /** relegates to objectParser */
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = objectLevel.apply(pu)
}
