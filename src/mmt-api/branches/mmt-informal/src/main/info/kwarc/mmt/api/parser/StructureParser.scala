package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import symbols._
import patterns._
import objects._

import scala.collection.mutable.{ListMap,HashMap}

/** 
 * This class bundles all state that is maintained by a [[info.kwarc.mmt.api.parser.StructureParser]]
 * 
 * @param reader the input stream, from which the parser reads
 * @param container the MMT URI of the input stream (used for back-references)
 */
class ParserState(val reader: Reader, val container: DPath) {
   /**
    * the namespace mapping set by
    * {{{
    * import prefix URI
    * }}}
    * commands
    */
   var namespaces = new utils.NamespaceMap
   namespaces.default = utils.mmt.mmtbase.uri

   /** uri at current parsing location */
   var home : Path = DPath(namespaces.default)
   
   /** the position at which the current StructuralElement started */ 
   var startPosition = reader.getSourcePosition
   
   /** all errors encountered during parsing, in reverse order */()
   var errors : List[SourceError] = Nil
   /** all errors encountered during parsing */()
   def getErrors = errors.reverse
   
   def copy(reader: Reader = reader) = {
      val s = new ParserState(reader, container)
      s.namespaces = namespaces
      s
   }
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
    * @param document the current document
    * @param keyword the keyword that was read
    * 
    *  the keyword but nothing else has been read already when this is called
    */
   def apply(sp: StructureParser, s: ParserState, document: Document, keyword: String)
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
    * @param theory the current theory
    * @param keyword the keyword that was read

    * the keyword but nothing else has been read already when this is called
    */
   def apply(sp: StructureParser, s: ParserState, theory: DeclaredTheory, keyword: String)
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
   def seCont(se: StructuralElement)(implicit state: ParserState): Unit
   /**
    * A continuation function called on every ParsingUnit that was found
    * 
    * Objects are opaque to the parser, and parsing is deferred via this function.
    */
   def puCont(pu: ParsingUnit)(implicit state: ParserState): Term
   /**
    * A continuation function called on every error that was found
    * 
    * Error handling is very lenient and will recover wherever possible.
    * 
    * Default implementation adds every error to the ParserState
    */
   def errorCont(err: SourceError)(implicit state: ParserState) {
      state.errors ::= err
   }
   
   /** called at the end of a document or module, does common bureaucracy */
   protected def end(s: StructuralElement)(implicit state: ParserState) {
      //extend source reference until end of element
      SourceRef.get(s) foreach {r => 
         SourceRef.update(s, r.copy(region = r.region.copy(end = state.reader.getSourcePosition)))
      }
      log("end " + s.path)
   }
   
   /** the main interface function
    * 
    * @param r a Reader holding the input stream
    * @param dpath the MMT URI of the stream
    */
   def apply(r: Reader, dpath: DPath) : (Document,ParserState) = {
      val state = new ParserState(r, dpath)
      apply(state, dpath)
   }
   
   def apply(state : ParserState, dpath : DPath) : (Document,ParserState) = {
      state.namespaces.default = dpath.uri
      state.home = dpath
      val doc = new Document(dpath)
      seCont(doc)(state)
      logGroup {
         readInDocument(doc)(state)
      }
      log("end " + dpath)
      (doc,state)
   }

   /** convenience function to create SourceError's */
   private def makeError(reg: SourceRegion, s: String) =
      SourceError("structure-parser", SourceRef(null, reg), s)
  
   /** the region from the start of the current structural element to the current position */
   protected def currentSourceRegion(implicit state: ParserState) =
      SourceRegion(state.startPosition, state.reader.getSourcePosition)
      
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
      val sexp = state.namespaces.expand(s)
      try {Path.parseD(sexp, base)}
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
      val sexp = state.namespaces.expand(s)
      try {Path.parseM(sexp, base)}
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
      val sexp = state.namespaces.expand(s)
      try {Path.parseS(sexp, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /**
    * reads one out of a list of permitted delimiters
    * @param delims the permitted delimiter
    * @return the read delimiter
    * @throws SourceError iff anything else found
    */
   def readDelimiter(delims: String*)(implicit state: ParserState): String = {
      val delim = state.reader.readToken
      if (delims.contains(delim._1))
         delim._1
      else
         throw makeError(delim._2, delims.map("'" + _ + "'").mkString("" ," or ", "") + "expected")
   }
   
   def readParsedObject(scope: Term, context: Context = Context())(implicit state: ParserState) = {
      val (obj, reg) = state.reader.readObject
      val pu = ParsingUnit(SourceRef(state.container.uri, reg), scope, Context(), obj)
      val parsed = puCont(pu)
      (obj,reg,parsed)
   }
  
   /** the main loop for reading declarations that can occur in documents
    * @param doc the containing Document (must be in the controller already)
    */
   private def readInDocument(doc: Document)(implicit state: ParserState) {
      state.home = doc.path
      //auxiliary function to unify "view" and "implicit view"
      def doView(isImplicit: Boolean) {
    	       
               val name = readLocalPath
               val ns = DPath(state.namespaces.default)
               val vpath = ns ? name
               val mref = MRef(doc.path, vpath, true)
               seCont(mref)
               readDelimiter(":")
               val from = readMPath(vpath)
               readDelimiter("->","→")
               val to = readMPath(vpath)
               readDelimiter("abbrev", "=") match {
                  case "abbrev" =>
                     val (_,_,df) = readParsedObject(OMMOD(vpath))
                     val thy = new DefinedView(ns, name, OMMOD(from), OMMOD(to), df, isImplicit)
                     seCont(thy)
                  case "=" =>
                     val v = new DeclaredView(ns, name, OMMOD(from), OMMOD(to), isImplicit)
                     seCont(v)
                     logGroup {
                        readInView(v)
                     }
                     end(v)
               }
      }
      if (state.reader.endOfDocument) return
      val (keyword, reg) = state.reader.readToken
      state.startPosition = reg.start
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
               end(d)
            case "namespace" =>
               val ns = readDPath(doc.path)
               state.namespaces.default = ns.uri
            case "import" =>
               val (n,_) = state.reader.readToken
               val ns = readDPath(doc.path)
               state.namespaces.prefixes(n) = ns.uri
            case "theory" =>
               val name = readLocalPath
               val ns = DPath(state.namespaces.default)
               val tpath = ns ? name
               val mref = MRef(doc.path, tpath, true)
               seCont(mref)
               var delim = state.reader.readToken
               if (delim._1 == "abbrev") {
                  val (_,_,df) = readParsedObject(OMMOD(tpath))
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
                     val patterns: List[(String,GlobalName)] = meta match {
                        case None => Nil
                        case Some(mt) =>
                           try {
                              //TODO this does not cover imported patterns
                              controller.globalLookup.getDeclaredTheory(mt).getPatterns.map {
                                 p => (p.name.toPath, p.path)
                              }
                           } catch {case e: Error =>
                              errorCont(makeError(reg, "error while retrieving patterns, continuing without patterns"))
                              Nil
                           }
                     }
                     logGroup {
                        readInTheory(t, patterns)
                     }
                     end(t)
                  } else {
                     throw makeError(delim._2, "':' or '=' or 'abbrev' expected")
                  }
               }
            case "view" | "morphism" => doView(false)
            case "implicit" =>
               val (keyword2, reg2) = state.reader.readToken
               keyword2 match {
                  case "view" | "morphism" => doView(true)
                  case _ => throw makeError(reg2, "only views can be implicit here")
               }
            case k =>
               // other keywords are treated as parser plugins
               val extParser = inDocParsers.get(k).getOrElse {
                  throw makeError(reg, "unknown keyword: " + k)
               }
               val (mod, mreg) = state.reader.readModule
               val reader = Reader(mod)
               reader.setSourcePosition(mreg.start)
               extParser(this, state.copy(reader), doc, k)
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
      state.home = thy.path
      //this case occurs if lower methods have already read the GS marker
      if (state.reader.endOfModule) return
      try {
         val (keyword, reg) = state.reader.readToken
         state.startPosition = reg.start
         def fail(s: String) = throw makeError(reg,s)
         keyword match {
            //this case occurs if we read the GS or marker
            case "" =>
               if (state.reader.endOfModule) {
                  return
               } else
                  fail("keyword expected, within theory " + thy.path)
            //Constant
            case "constant" =>
               val name = readName
               val c = readConstant(name, thy.path, OMMOD(thy.path))
               seCont(c)
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
                     fail("instance declaration illegal without meta-theory")
                  case Some(mt) =>
                     val pattern = readSPath(mt)
                     readInstance(name, thy.path, pattern)
               }
            case k =>
               // other keywords are treated as ...
               val patOpt = patterns.find(_._1 == k)
               if (patOpt.isDefined) {
                  // 1) an instance of a Pattern with LocalName k visible in meta-theory 
                  val pattern = patOpt.get._2
                  val name = readName
                  readInstance(name, thy.path, pattern)
               } else {
                  val parsOpt = inTheoryParsers.get(k)
                  if (parsOpt.isDefined) {
                     // 2) a parser plugin identified by k
                     val (decl, reg) = state.reader.readDeclaration
                     val reader = Reader(decl)
                     reader.setSourcePosition(reg.start)
                     parsOpt.get.apply(this, state.copy(reader), thy, k)
                  } else {
                     // 3) a constant with name k
                     val name = LocalName.parse(k)
                     val c = readConstant(name, thy.path, OMMOD(thy.path))
                     seCont(c)
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
   private def readConstant(name: LocalName, parent: MPath, scope: Term)(implicit state: ParserState): Constant = {
      val cpath = parent ? name
      //initialize all components as omitted
      val tpC = new TermContainer
      var dfC = new TermContainer
      var al : Option[LocalName] = None
      var nt : Option[TextNotation] = None
      var rl : Option[String] = None
      var pr : Option[Context] = None
      // every iteration reads one delimiter and one object
      // @ alias or : TYPE or = DEFINIENS or # NOTATION
      //TODO remove "##" here and in the case split below, only used temporarily for latex integration
      val keys = List(":","=","#", "##","@", "of", "role")
      val keyString = keys.map("'" + _ + "'").mkString(", ")

      while (! state.reader.endOfDeclaration) {
         val (delim, treg) = state.reader.readToken
         if (! keys.contains(delim)) {
            // error handling
            if (delim == "") {
               if (! state.reader.endOfDeclaration)
                  errorCont(makeError(treg, "expected " + keyString))
            } else { 
               if (! state.reader.endOfObject)
                  state.reader.readObject
               errorCont(makeError(treg, "expected " + keyString + ", ignoring the next object"))
            }
         } else {
            def doComponent(c: DeclarationComponent, tc: TermContainer) {
               val (obj,_,tm) = readParsedObject(scope, pr.getOrElse(Context()))
               tc.read = obj
               tc.parsed = tm
            }
            // the main part, which branches based on the delimiter
            delim match {
               case "of" =>
                  if (pr.isDefined) {
                     errorCont(makeError(treg, "parameters of this constant already given, ignored"))
                     state.reader.readObject
                  } else {
                     val (obj, reg) = state.reader.readObject
                     val pu = ParsingUnit(SourceRef(state.container.uri, reg), scope, Context(), obj, Some(TextNotation.contextNotation))
                     val parsed = puCont(pu)
                     parsed match {
                        case OMBINDC(utils.mmt.context, cont, Nil) =>
                           pr = Some(cont)
                        case _ =>
                           errorCont(makeError(reg, "parameters of this constant are not a context, ignored (note that implicit parts are not allowed in parameters)"))
                     }
                  }
               case ":" =>
                  if (tpC.read.isDefined) {
                     errorCont(makeError(treg, "type of this constant already given, ignored"))
                     state.reader.readObject
                  } else
                     doComponent(TypeComponent, tpC)
               case "=" =>
                  if (dfC.read.isDefined) {
                     errorCont(makeError(treg, "definiens of this constant already given, ignored"))
                     state.reader.readObject
                  } else
                     doComponent(DefComponent, dfC)
               case "#" | "##" =>
                  val (notString,reg) = state.reader.readObject
                  if (nt.isDefined)
                     errorCont(makeError(treg, "notation of this constant already given, ignored"))
                  else {
                     val notation = TextNotation.parse(notString, cpath)
                     nt = Some(notation)
                  }
               case "@" =>
                  val (str,_) = state.reader.readObject
                  if (al.isDefined)
                     errorCont(makeError(treg, "alias of this constant already given, ignored"))
                  else {
                     al = Some(LocalName.parse(str))
                  }
               case "role" =>
                 val (str,_) = state.reader.readObject
                 rl = Some(str)                 
               //TODO read metadata
            }
         }
      }
      new Constant(OMMOD(parent), name, al, tpC, dfC, rl, nt) {
         override val parameters = pr.getOrElse(Context())
      }
   }
   private def readInstance(name: LocalName, tpath: MPath, pattern: GlobalName)(implicit state: ParserState) {
      val args = if (state.reader.endOfDeclaration)
         Nil
      else { 
         val (obj,reg,tm) = readParsedObject(OMMOD(tpath))
         controller.pragmatic.pragmaticHead(tm) match {
            case OMAMaybeNil(OMID(`pattern`), args) => args
            case _ => throw makeError(reg, "not an instance of pattern " + pattern.toPath)
         }
      }
      val instance = new Instance(OMMOD(tpath), name, pattern, args)
      seCont(instance)
   }
   
   private def readInView(view: DeclaredView)(implicit state: ParserState) {
      //this case occurs if lower methods have already read the GS marker
      if (state.reader.endOfModule) return
      try {
         val (keyword, reg) = state.reader.readToken
         state.startPosition = reg.start
         keyword match {
            //this case occurs if we read the GS or marker
            case "" =>
               if (state.reader.endOfModule) {
                  return
               } else
                  throw makeError(reg, "keyword expected, within view " + view.path).copy(fatal = true)
            //Constant
            case "constant" =>
               val name = readName
               val a = readConstant(name, view.path, view.to).toConstantAssignment
               seCont(a)
            //assignment to include
            case "include" =>
               log("found include")
               val from = readMPath(view.path)
               readDelimiter("=")
               val mor = OMMOD(readMPath(view.path)) //readParsedObject(view.to)
               val as = new DefLinkAssignment(view.toTerm, LocalName(ComplexStep(from)), from, mor)
               seCont(as)
            //Pattern
            case "pattern" =>
               //TODO
            //Instance
            case "instance" =>
               //TODO
            case k =>
               //TODO
               // other keywords are treated as ...
               /*
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
                */
                     // 3) a constant with name k
                     val name = LocalName.parse(k)
                     val a = readConstant(name, view.path, view.to).toConstantAssignment
                     seCont(a)
         }
      } catch {
         case e: SourceError =>
            errorCont(e)
            if (! state.reader.endOfDeclaration)
               state.reader.readDeclaration
      }
      readInView(view) // compiled code is not actually tail-recursive
   }
   
   //TODO, text syntax for styles?
   //def readInStyle(style: MPath)(implicit state: ParserState) {}
   
   /** API Functions **/
   
   /**
    * Reads a document from a string
    * @param docURI the base uri (location) of the document
    * @param docS the document string
    */
   def readDocument(docURI : DPath, docS : String) {
     val r = new Reader(new java.io.BufferedReader(new java.io.StringReader(docS)))
     apply(r, docURI)
     r.close
   }
		   			
   /**
    * reads a module from a string
    * @param dpath the uri of the parent document
    * @param modS the module string
    * @param docBaseO optionally the base uri of the containing document 
    * @param namespace the namespace declarations found so far
    */
   def readModule(dpath : DPath, 
		   	      modS : String, 
                  docBaseO : Option[DPath] = None, 
                  namespace : ListMap[String, DPath] = new ListMap) {
     //building parsing state
     val r = new Reader(new java.io.BufferedReader(new java.io.StringReader(modS)))
     val state = new ParserState(r, docBaseO.getOrElse(null))
     docBaseO.map(dp => state.namespaces.default = dp.uri)
     namespace map { p =>
       state.namespaces.prefixes(p._1) = p._2.uri
     }
     val doc = controller.getDocument(dpath)
     //calling parse function
     readInDocument(doc)(state)
     r.close
   }
   
   /**
    * reads a constant from a string
    * @param mpath the uri of the parent module
    * @param modS the constant string
    * @param docBaseO optionally the base uri of the containing document 
    * @param namespace the namespace declarations found so far
    */
   def readConstant(mpath : MPath, 
		   	        conS : String, 
                    docBaseO : Option[DPath] = None, 
                    namespace : ListMap[String, DPath] = new ListMap) = {
     //building parsing state
     val r = new Reader(new java.io.BufferedReader(new java.io.StringReader(conS)))
     val state = new ParserState(r, docBaseO.getOrElse(null))
     docBaseO.map(dp => state.namespaces.default = dp.uri)
     namespace map { p =>
       state.namespaces.prefixes(p._1) = p._2.uri
     }
     
     val thy = controller.globalLookup.getTheory(mpath) match {
       case d : DeclaredTheory => d
       case _ => throw ImplementationError("Expected declared theory at path" + mpath)
     }
     val patterns = Nil //TODO
     //calling parse function
     
     readInTheory(thy, patterns)(state)
   }
}

/**
 * An implementation of the continuation functions in StructureParser with reasonable defaults.
 */
class StructureAndObjectParser(controller: Controller) extends StructureParser(controller) {
   /**
    * parsing units are parsed by the termParser of the controller
    * if that fails, the [[info.kwarc.mmt.api.parser.DefaultObjectParser]] is used
    */
   def puCont(pu: ParsingUnit)(implicit state: ParserState): Term = {
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
   def seCont(se: StructuralElement)(implicit state: ParserState) {
      log(se.toString)
      //log(se.toNode.toString)
      SourceRef.update(se, SourceRef(state.container.uri,currentSourceRegion))
      controller.add(se)
   }
}


class SequentialReader extends java.io.Reader {
  var text : List[Char] = Nil
  
  def appendLine(line : String) = {
    text ++= "\n".toCharArray.toList 
    text ++= line.toCharArray().toList
  }
  
  
  def read(cbuf : Array[Char], offset : Int, len : Int) : Int = {
    while (!ready) 
      Thread.sleep(10)
    var i = 0
    while (i < len && (offset + i) < text.length) {
      cbuf(i) = text.head
      text = text.tail
      i += 1
    }
    i
  }
  
  def close() = {
    //nothing to do
  }
  
  def isDone = text.isEmpty
    
  override def ready() = !text.isEmpty
}

class SeqBufReader(in: SequentialReader = new SequentialReader) extends java.io.BufferedReader(in) {
  def appendLine(line : String) = in.appendLine(line)
  
  def isDone = in.isDone
}



