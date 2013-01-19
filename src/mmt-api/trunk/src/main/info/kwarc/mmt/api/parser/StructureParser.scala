package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import symbols._
import objects._

import scala.collection.mutable.{ListMap,HashMap}

/** 
 * This class bundles all state that is maintained by a [[info.kwarc.mmt.api.parser.StructureParser]]
 * 
 * @param reader the input stream, from which the parser reads
 */
case class ParserState(val reader: Reader) {
   /**
    * the namespace mapping set by
    * {{{
    * import prefix URI
    * }}}
    * commands
    */
   val namespace = new ListMap[String,DPath]
   /**
    * the default namespace set by
    * {{{
    * namespace URI
    * }}}
    * commands
    */
   var defaultNamespace: DPath = utils.mmt.mmtbase
}

/**
 * classes implementing InDocParser may be registered with a [[info.kwarc.mmt.api.parser.StructureParser]]
 * to extend MMT
 */
trait InDocParser {
   /**
    * Called to parse a declaration in a Document if the respective keyword has been read.
    * @param sp the StructureParser that is calling this extension
    * @param r the reader from which further input can be read
    * 
    *  the keyword but nothing else has been read already when this is called
    */
   def apply(sp: StructureParser, r: Reader)
}

/**
 * classes implementing InTheoryParser may be registered with a [[info.kwarc.mmt.api.parser.StructureParser]]
 * to extend MMT
 */
trait InTheoryParser {
   /**
    * Called to parse a declaration in a Document if the respective keyword has been read.
    * @param sp the StructureParser that is calling this extension
    * @param r the reader from which further input can be read
    * 
    *  the keyword but nothing else has been read already when this is called
    */
   def apply(sp: StructureParser, r: Reader)
}


/**
 * A StructureParser read MMT declarations (but not objects) and
 * defers to continuation functions for the found StructuralElement, ParsingUnits, and SourceErrors.
 * 
 * This class provides 3 things
 * 
 * 1) High-level read methods that read MMT-related entities from a stream,
 * which implementing classes can use.
 * These methods throw do not read more than necessary from the stream and
 * throw [[info.kwarc.mmt.api.SourceError]] where appropriate.
 * 
 * 2) It maintains the parse state via an implicit argument
 * [[info.kwarc.mmt.api.parser.ParserState]] in most functions.
 * 
 * 3) It leaves processing of MMT entities application-independent via high-level continuation functions. 
 */
abstract class StructureParser(controller: Controller) extends frontend.Logger {
   val report = controller.report
   val logPrefix = "structure-parser"
   
   /**
    * a table of external parsers that can parser declarations in a document, indexed by keyword
    * 
    *  plugins may register parsers here to extend MMT with new keywords
    */
   val inDocParsers = new HashMap[String,InDocParser]
   /**
    * a table of external parsers that can parser declarations in a theory, indexed by keyword
    * 
    *  plugins may register parsers here to extend MMT with new keywords
    */
   val inTheoryParsers = new HashMap[String,InTheoryParser]
   
   /**
    * A continuation function called on every StructuralElement that was found
    * 
    * For grouping elements (documents, modules with body), this is called on the empty element first
    * and then on each child.
    */
   def seCont(se: StructuralElement): Unit
   /**
    * A continuation function called on every ParsingUnit that was found
    * 
    * Objects are opaque to the parser, and parsing is deferred via this function.
    */
   def puCont(pu: ParsingUnit): Term
   /**
    * A continuation function called on every error that was found
    * 
    * Error handling is very lenient and will recover wherever possible.
    */
   def errorCont(err: SourceError): Unit
   
   /** the main interface function
    * 
    * @param r a Reader holding the input stream
    * @param dpath the MMT URI of the stream
    */
   def apply(r: Reader, dpath: DPath) {
      val state = new ParserState(r)
      state.defaultNamespace = dpath
      val doc = new Document(dpath)
      seCont(doc)
      logGroup {
         readInDocument(doc)(state)
      }
      log("end " + dpath)
   }

   /** convenience function to create SourceError's */
   private def makeError(reg: SourceRegion, s: String) =
      SourceError("structure-parser", SourceRef(null, reg), s)
   
   /** read a LocalName from the stream
    * @throws SourceError iff ill-formed or empty
    */
   def readName(implicit state: ParserState) : LocalName = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "name expected")
      try {LocalName.parse(s)}
      catch {case e: ParseError =>
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /** read a LocalPath from the stream
    * @throws SourceError iff ill-formed or empty
    */
   def readLocalPath(implicit state: ParserState) : LocalPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "module name expected")
      try {Path.parseLocal(s).toLocalPath}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /** read a DPath from the stream
    * @throws SourceError iff ill-formed or empty
    */
   def readDPath(base: Path)(implicit state: ParserState) : DPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseD(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /** read a MPath from the stream
    * @throws SourceError iff ill-formed or empty
    */
  def readMPath(base: Path)(implicit state: ParserState) : MPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseM(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
  /** read a GlobalName from the stream
    * @throws SourceError iff ill-formed or empty
    */
  def readSPath(base: MPath)(implicit state: ParserState) : GlobalName = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseS(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /** the main loop for reading declarations that can occur in documents
    * @param doc the containing Document (must be in the controller already)
    */
   private def readInDocument(doc: Document)(implicit state: ParserState) {
      if (state.reader.endOfDocument) return
      val (keyword, reg) = state.reader.readToken
      try {
         keyword match {
            case "" =>
               if (state.reader.endOfDocument) {
                  return
               } else
                  throw makeError(reg, "keyword expected, within document " + doc).copy(fatal = true)
            case "document" =>
               val name = readLocalPath
               val dpath = doc.path / name
               val d = new Document(dpath)
               seCont(d)
               val dref = DRef(doc.path, dpath)
               seCont(dref)
               logGroup {
                  readInDocument(d)
               }
               log("end " + dpath)
            case "namespace" =>
               val ns = readDPath(doc.path)
               state.defaultNamespace = ns 
            case "import" =>
               val (n,_) = state.reader.readToken
               val ns = readDPath(doc.path)
               state.namespace(n) = ns
            //case "link" => readLink //TODO
            //case "meta" => readMetaDatum //TODO
            case "theory" =>
               val name = readLocalPath
               val ns = state.defaultNamespace
               val tpath = ns ? name
               val mref = MRef(doc.path, tpath)
               seCont(mref)
               var delim = state.reader.readToken
               if (delim._1 == "abbrev") {
                  val (obj, reg) = state.reader.readObject
                  val df = puCont(ParsingUnit(tpath $ DefComponent, OMMOD(tpath), Context(), obj))
                  val thy = new DefinedTheory(ns, name, df)
                  seCont(thy)
               } else {
                  val meta = if (delim._1 == ":") {
                     val p = readMPath(tpath)
                     delim = state.reader.readToken
                     Some(p)
                  } else
                     None
                  val t = new DeclaredTheory(ns, name, meta)
                  seCont(t)
                  if (delim._1 == "=") {
                     val patterns: List[(String,GlobalName)] = Nil //Theory.getPatterns(mt)
                     logGroup {
                        readInTheory(t, patterns)
                     }
                     log("end " + tpath)
                  } else {
                     throw makeError(delim._2, "':' or '=' or 'abbrev' expected")
                  }
               }
            case k =>
               // other keywords are treated as parser plugins
               val extParser = inDocParsers.get(k).getOrElse {
                  throw makeError(reg, "unknown keyword: " + k)
               }
               val (mod, mreg) = state.reader.readModule
               val reader = Reader(mod)
               reader.setSourcePosition(mreg.start)
               extParser(this, reader)
         }
      } catch {
         case e: SourceError =>
            errorCont(e)
            if (! state.reader.endOfModule)
               state.reader.readModule
      }
      readInDocument(doc) // compiled code is not actually tail-recursive
   }
   
   /** the main loop for reading declarations that can occur in a theory
    * @param thy the containing theory (must be in the controller already)
    * @param patterns the patterns of the meta-theory (precomputed in readInDocument)
    * 
    * this function handles one declaration if possible, then calls itself recursively
    */
   private def readInTheory(thy: DeclaredTheory, patterns: List[(String,GlobalName)])(implicit state: ParserState) {
      //this case occurs if lower methods have already read the GS marker
      if (state.reader.endOfModule) return
      try {
         val (keyword, reg) = state.reader.readToken
         keyword match {
            //this case occurs if we read the GS or marker
            case "" =>
               if (state.reader.endOfModule) {
                  return
               } else
                  throw makeError(reg, "keyword expected, within theory " + thy).copy(fatal = true)
            //Constant
            case "constant" =>
               val name = readName
               readConstant(name, thy.path)
            //PlainInclude
            case "include" =>
               val from = readMPath(thy.path)
               if (! state.reader.endOfDeclaration) {
                  val (rest, reg) = state.reader.readDeclaration
                  if (rest != "")
                     throw makeError(reg, "end of declaration expected, found and ignored " + rest)
               }
               val incl = PlainInclude(from, thy.path)
               seCont(incl)
            //Pattern
            case "pattern" =>
               val name = readName
               //TODO
            //Instance
            case "instance" =>
               val name = readName
               thy.meta match {
                  case None =>
                     throw makeError(reg, "instance declaration illegal without meta-theory")
                  case Some(mt) =>
                     val pattern = readSPath(mt)
                     readInstance(name, pattern)
               }
            case k =>
               // other keywords are treated as ...
               val patOpt = patterns.find(_._1 == k)
               if (patOpt.isDefined) {
                  // 1) an instance of a Pattern with LocalName k visible in meta-theory 
                  val pattern = patOpt.get._2
                  val name = readName
                  readInstance(name, pattern)
               } else {
                  val parsOpt = inTheoryParsers.get(k)
                  if (parsOpt.isDefined) {
                     // 2) a parser plugin identified by k
                     val (decl, reg) = state.reader.readDeclaration
                     val reader = Reader(decl)
                     reader.setSourcePosition(reg.start)
                     parsOpt.get.apply(this, reader)
                  } else {
                     // 3) a constant with name k
                     val name = LocalName.parse(k)
                     readConstant(name, thy.path)
                  }
               }
         }
      } catch {
         case e: SourceError =>
            errorCont(e)
            if (! state.reader.endOfDeclaration)
               state.reader.readDeclaration
      }
      readInTheory(thy, patterns) // compiled code is not actually tail-recursive
   }
   
   /** reads the components of a [[info.kwarc.mmt.api.symbols.Constant]]
    * @param name the name of the constant
    * @param the containing [[info.kwarc.mmt.api.modules.DeclaredTheory]]
    */
   private def readConstant(name: LocalName, tpath: MPath)(implicit state: ParserState) {
      val cpath = tpath ? name
      //initialize all components as omitted
      var tp : Option[Term] = None
      var df : Option[Term] = None
      var al : Option[LocalName] = None
      var nt : Option[TextNotation] = None
      // every iteration reads one delimiter and one object
      // : TYPE or = DEFINIENS or # NOTATION 
      while (! state.reader.endOfDeclaration) {
         val (delim, treg) = state.reader.readToken
         if (! List(":","=","#","@").contains(delim)) {
            // error handling
            if (delim == "") {
               if (! state.reader.endOfDeclaration)
                  errorCont(makeError(treg, "expected ':' or '=' or '#'"))
            } else { 
               if (! state.reader.endOfObject)
                  state.reader.readObject
               errorCont(makeError(treg, "expected ':' or '=' or '#', ignoring the next object"))
            }
         } else {
            val (obj, oreg) = state.reader.readObject
            def doComponent(c: DeclarationComponent) = {
               val tm = puCont(ParsingUnit(cpath $ c, OMMOD(tpath), Context(), obj))
               Some(tm)
            }
            // the main part, which branches based on the delimiter
            delim match {
               case ":" =>
                  if (tp.isDefined)
                     errorCont(makeError(oreg, "type of this constant already given, ignored"))
                  else
                     tp = doComponent(TypeComponent)
               case "=" =>
                  if (df.isDefined)
                     errorCont(makeError(oreg, "definiens of this constant already given, ignored"))
                  else
                     df = doComponent(DefComponent)
               case "#" =>
                  if (nt.isDefined)
                     errorCont(makeError(oreg, "notation of this constant already given, ignored"))
                  else {
                     val notString = obj
                     val notation = TextNotation.parse(notString, cpath)
                     nt = Some(notation)
                  }
               case "@" =>
                  if (al.isDefined)
                     errorCont(makeError(oreg, "alias of this constant already given, ignored"))
                  else
                     al = Some(LocalName.parse(obj))
               //TODO read metadata
            }
         }
      }
      val c = new Constant(OMMOD(tpath), name, al, tp, df, None, nt)
      seCont(c)
   }
   private def readInstance(name: LocalName, pattern: GlobalName)(implicit state: ParserState) {
      
   }
   
   private def readInView(view: MPath)(implicit state: ParserState) {
      
   }
   
   //TODO, text syntax for styles?
   //def readInStyle(style: MPath)(implicit state: ParserState) {}
}

/**
 * An implementation of the continuation functions in StructureParser with reasonable defaults.
 */
class StructureAndObjectParser(controller: Controller) extends StructureParser(controller) {
   /**
    * parsing units are parsed by the termParser of the controller
    * if that fails, the [[info.kwarc.mmt.api.parser.DefaultParser]] is used
    */
   def puCont(pu: ParsingUnit): Term = {
      val obj = try {
         controller.termParser(pu)  
      } catch {
         case e: SourceError =>
            errorCont(e)
            DefaultObjectParser(pu)
      }
      obj
   }
   /**
    * structural elements are added to controller
    */
   def seCont(se: StructuralElement) {
      log(se.toString)
      controller.add(se)
   }
   private var errors: List[SourceError] = Nil
   /**
    * errors are reported and stored
    */
   def errorCont(err: SourceError) {
      errors ::= err
      report(err)
   }
   /** returns all errors since the last reset, in order of occurrence */
   def getErrors = errors.reverse
   /** resets the error list */
   def resetErrors {errors = Nil}
}

