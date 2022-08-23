package info.kwarc.mmt.api.parser

import java.io.BufferedReader

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend._
import objects._
import utils._

/** ParsingUnit encapsulates the input of an [[ObjectParser]]
  *
  * the top parameter can be used to parse a term that is known/required to have a certain form
  *
  * @param source the source reference of the string to parse
  * @param context the context against which to parse
  * @param term the term to parse
  * @param top an optional notation that the whole input must match;
  */
case class ParsingUnit(source: SourceRef, context: Context, term: String, iiContext: InterpretationInstructionContext, top: Option[ParsingRule] = None) extends MMTTask {
   /** level determines the notation extension: approximated by the largest known containing theory */
   def getLevel = {
     val levelCandidates = context.getIncludes
     levelCandidates.filter(_.name.length == 1).lastOption.getOrElse(levelCandidates.last)
   }
   def nsMap = iiContext.namespaces
}
// TODO top should be Option[GlobalName]

/** encapsulates the output of an [[ObjectParser]]
 *  @param unknown the unknown variables that must be solved
 *  @param free the free variables that must be bound at the outside (may use unknowns)
 *  @param term the parsed term (may use unknowns and free variables)
 */
case class ParseResult(unknown: Context, free: Context, term: Term) {
   def toTerm = {
      var res = term
      if (free.nonEmpty) {
         res = Free(free, res)
      }
      if (unknown.nonEmpty) {
         res = OMBIND(OMS(ParseResult.unknown), unknown, res)
      }
      SourceRef.get(term).foreach(SourceRef.update(res,_))
      res
  }
   /** true if no unknowns/free variables found */
  def isPlainTerm = unknown.isEmpty && free.isEmpty

  /** replaces the term */
  def map(f: Term => Term) = copy(term = f(term))
}

object ParseResult {
   val unknown = utils.mmt.mmtcd ? "unknown"
   val free = Free.free
   val substitute = utils.mmt.mmtcd ? "substitute"
   def fromTerm(uft: Term) = {
      val (u,ft) = uft match {
         case OMBIND(OMS(this.unknown), u, ft) => (u, ft)
         case _ => (Context.empty, uft)
      }
      val (f,t) = ft match {
         case Free(f, t) => (f,t)
         case _ => (Context.empty, ft)
      }
      ParseResult(u,f,t)
   }

  object VariablePrefixes {
    val implicitArg = LocalName("") / "I"
    val explicitUnknown = LocalName("") / "_"
  }
}

/** passed to [[Parser]]s and [[checking.Interpreter]]s to indicate the place inside a larger element the input is located */
sealed abstract class ParentInfo
/** abstraction to unify operations inside a root document or module */
sealed abstract class RootInfo extends ParentInfo {
   /** the path of this element */
   val path: Path
}
/** the content is a root document */
case class IsRootDoc(path: DPath) extends RootInfo
/** the content is a root module */
case class IsRootMod(path: MPath) extends RootInfo

/** abstraction to unify operations inside a document and inside a module */
sealed abstract class HasParentInfo extends ParentInfo {
   def docParent: DPath
}
/** the content is located inside a document
 *  @param docParent the parent document
 */
case class IsDoc(docParent: DPath) extends HasParentInfo
/** the content is located inside a ModuleOrLink
 *  @param modParent the parent module
 *  @param relDocParent the path of the parent document relative to the parent module
 */
case class IsMod(modParent: MPath, relDocParent: LocalName) extends HasParentInfo {
   /** the parent document */
   def docParent = modParent.toDPath / relDocParent
}


/** ParsingStream encapsulates the input of a [[StructureParser]]
  *
  * @param source the URI of the stream
  * @param parentInfo information about the parent (if any) of the content in the stream (which - if given - must exist)
  * @param nsMap defined namespaces
  * @param format the format of the stream
  * @param stream the stream to parse
  */
case class ParsingStream(source: URI, parentInfo: ParentInfo, nsMap: NamespaceMap, format: String, stream: java.io.BufferedReader) extends MMTTask {
  /** the whole stream as a string */
  def fullString = Stream.continually(stream.readLine()).takeWhile(_ != null).mkString("\n")
}

object ParsingStream {
  /** to allow passing a string instead of a reader */
  implicit def stringToReader(s: String): BufferedReader = new java.io.BufferedReader(new java.io.StringReader(s))

  /** to allow passing a file instead of a reader */
  implicit def fileToReader(f: File): BufferedReader = File.Reader(f)

  /** creates a ParsingStream from a file, making reasonable default choices
    *
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
    new ParsingStream(FileURI(f), IsRootDoc(dpath), nsMap, format, stream)
  }

  /** creates a ParsingStream from a string
    *
    * @param s the string
    * @param dpath the logical URI of the string
    * @param format format
    * @param nsMapOpt the namespace map
    */
  def fromString(s: String, dpath: DPath, format: String, nsMapOpt: Option[NamespaceMap] = None) = {
    val nsMap = nsMapOpt.getOrElse(NamespaceMap(dpath))
    new ParsingStream(dpath.uri, IsRootDoc(dpath), nsMap, format, s)
  }

  /** creates a parsing stream for a source file in an archive
    *
    * return a parsing stream where
    * source: logical document URI with native file extension
    * dpath: logical document URI with "omdoc" file extension
    * format: file extension

    * @param a an archive
    * @param inPath path to the source file
    * @param strOpt the reader to use (defaults to file reader)
    *
    * @return the parsing stream
    */
  def fromSourceFile(a: Archive, inPath: FilePath, strOpt: Option[java.io.BufferedReader] = None, nsMapOpt: Option[NamespaceMap] = None): ParsingStream = {
    val inPathOMDoc = inPath.toFile.setExtension("omdoc").segments
    val base = a.narrationBase
    val dpath = DPath(base / inPathOMDoc) // bf.narrationDPath except for extension
    val stream = strOpt.getOrElse(File.Reader(a / source / inPath))
    val nsMap = nsMapOpt.getOrElse(NamespaceMap.empty) ++ a.namespaceMap
    ParsingStream(base / inPath.segments, IsRootDoc(dpath), nsMap(dpath), inPath.toFile.getExtension.getOrElse(""), stream)
  }
}

/** an ObjectParser handles ParsingUnits
  *
  * Instances are maintained by the ExtensionManager and retrieved and called by the structural parser.
  *
  * see also [[Parser]]
  *
  */
trait ObjectParser extends FormatBasedExtension {
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): ParseResult
}

/** helper object */
object ObjectParser {
  val oneOf = utils.mmt.mmtcd ? "oneOf"

  /** @return true if t is a result of parsing that may need further analysis */
  def isOnlyParsed(t: Term) = t.head.contains(ParseResult.unknown)
}

//TODO: notations should not be computed separately for each ParsingUnit; they must be cached theory-wise

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultObjectParser extends ObjectParser {
  def isApplicable(format: String) = true

  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = {
    val t = OMSemiFormal(objects.Text("unparsed", "\"" + pu.term + "\""))
    SourceRef.update(t, pu.source)
    ParseResult(Context.empty, Context.empty, t)
  }
}

/**
  * continuations that may be called by [[StructureParser]]s
  * @param errorCont called on errors
  */
class StructureParserContinuations(val errorCont: ErrorHandler) {
  /** to be called after parsing an element (but before parsing its body if any) */
  def onElement(se: StructuralElement): Unit = {}
  /** to be called after parsing the body of a [[ContainerElement]], e.g., documents and declared modules */
  def onElementEnd(se: ContainerElement[_]): Unit = {}
}

/** the type of structural parsers
  *
  * see also [[Parser]]
  */
trait StructureParser extends FormatBasedExtension {
  /** the main interface function: parses a stream and registers all elements (usually a single document) in it
    *
    * @param ps the encapsulated input stream
    * @param cont continuations for errors and parsed elements
    * @return the element into which the stream was parsed (of the type corresponding to ps.parentInfo)
    */
  def apply(ps: ParsingStream)(implicit cont: StructureParserContinuations): StructuralElement
}

/** the designated super class for all parsers */
abstract class Parser(val objectLevel: ObjectParser) extends StructureParser with ObjectParser with LeveledExtension {
  def format: String

  def isApplicable(s: String) = s == format

  /** relegates to objectParser */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = objectLevel.apply(pu)

  /** an interpreter that does not check */
  def asInterpreter = {
     val int = new checking.OneStepInterpreter(this)
     initOther(int)
     int
  }
}
