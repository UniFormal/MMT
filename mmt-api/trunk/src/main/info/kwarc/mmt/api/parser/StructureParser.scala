package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import notations._

import scala.collection.mutable.{ListMap,HashMap}

/** 
 * This class bundles all state that is maintained by a [[info.kwarc.mmt.api.parser.StructureParser]]
 * 
 * @param reader the input stream, from which the parser reads
 * @param container the MMT URI of the input stream (used for back-references)
 */
class ParserState(val reader: Reader, val container: DPath, val errorCont: ErrorHandler) {
   /**
    * the namespace mapping set by
    * {{{
    * import prefix URI
    * }}}
    * commands
    * 
    * the initial default namespace is the URI of the container
    */
   var namespaces = new utils.NamespaceMap
   namespaces.default = container.uri
   
   /** the position at which the current StructuralElement started */ 
   var startPosition = reader.getSourcePosition
   
   def copy(reader: Reader = reader) = {
      val s = new ParserState(reader, container, errorCont)
      s.namespaces = namespaces
      s
   }
}

/**
 * A StructureParser reads MMT declarations (but not objects) and
 * defers to continuation functions for the found StructuralElement, ParsingUnits, and SourceErrors.
 * 
 * This class provides 3 things
 * 
 * 1) High-level read methods that read MMT-related entities from a stream,
 * which implementing classes can use.
 * These methods throw do not read more than necessary from the stream and
 * throw [[info.kwarc.mmt.api.SourceError]] where appropriate.
 * 
 * 2) It is stateless and maintains the parse state via an implicit argument of type
 * [[info.kwarc.mmt.api.parser.ParserState]] in most functions.
 * 
 * 3) It leaves processing of MMT entities application-independently via high-level continuation functions. 
 */
class KeywordBasedParser(objectParser: ObjectParser) extends Parser(objectParser) {
   override val logPrefix = "structure-parser"
   
   /**
    * A continuation function called on every StructuralElement that was found
    * 
    * For grouping elements (documents, modules with body), this is called on the empty element first
    * and then on each child.
    */
   protected def seCont(se: StructuralElement)(implicit state: ParserState) {
      log(se.toString)
      val reg = currentSourceRegion
      SourceRef.update(se, SourceRef(state.container.uri,reg))
      try {controller.add(se)}
      catch {case e: Error =>
         val se = makeError(reg, e.getMessage)
         errorCont(se)
      }
   }
   /**
    * A continuation function called on every ParsingUnit that was found
    * 
    * Objects are opaque to the parser, and parsing is deferred to objectParser via this function.
    * 
    * Fatal errors are recovered from by defaulting to [[DefaultObjectParser]]
    */
   private def puCont(pu: ParsingUnit)(implicit state: ParserState): Term = {
      val obj = try {
         objectParser(pu)(state.errorCont)  
      } catch {
         case e: Error =>
            val se = makeError(pu.source.region, e.getMessage)
            state.errorCont(se)
            DefaultObjectParser(pu)(state.errorCont)
      }
      obj
   }
   /**
    * A continuation function called on every error that occurred
    */
   private def errorCont(e: SourceError)(implicit state: ParserState) = {
      state.errorCont(e)
   }
   
   /** called at the end of a document or module, does common bureaucracy */
   protected def end(s: StructuralElement)(implicit state: ParserState) {
      //extend source reference until end of element
      SourceRef.get(s) foreach {r => 
         SourceRef.update(s, r.copy(region = r.region.copy(end = state.reader.getSourcePosition)))
      }
      log("end " + s.path)
   }
   
   def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) : Document = {
      val (d, _) = apply(new ParserState(new Reader(ps.stream), ps.dpath, errorCont))
      d
   }
   
   private def apply(state : ParserState) : (Document,ParserState) = {
      val dpath = state.container
      state.namespaces.default = dpath.uri
      val doc = new Document(dpath)
      seCont(doc)(state)
      logGroup {
         readInDocument(doc)(state)
      }
      log("end " + dpath)
      (doc,state)
   }

   /** convenience function to create SourceError's */
   protected def makeError(reg: SourceRegion, s: String)(implicit state: ParserState) =
      SourceError("structure-parser", SourceRef(state.container.uri, reg), s)
  
   /** the region from the start of the current structural element to the current position */
   protected def currentSourceRegion(implicit state: ParserState) =
      SourceRegion(state.startPosition, state.reader.getSourcePosition)
      
   /** read a LocalName from the stream
    * @throws SourceError iff ill-formed or empty
    */
   def readName(implicit state: ParserState) : LocalName = {
      val (s, reg) = state.reader.readToSpace
      if (s == "")
         throw makeError(reg, "name expected")
      try {LocalName.parse(s)}
      catch {case e: ParseError =>
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   /** read a DPath from the stream
    * @throws SourceError iff ill-formed or empty
    */
   def readDPath(base: Path)(implicit state: ParserState) : DPath = {
      val (s, reg) = state.reader.readToSpace
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
      val (s, reg) = state.reader.readToSpace
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
      val (s, reg) = state.reader.readToSpace
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
      val pu = ParsingUnit(SourceRef(state.container.uri, reg), scope, context, obj)
      val parsed = puCont(pu)
      (obj,reg,parsed)
   }
  
  /** like seCont but may wrap in NestedModule if parent is MPath */
  private def moduleCont(m: Module, parent: Path)(implicit state: ParserState) {
      parent match {
         case _: DPath => seCont(m)
         case _: MPath => seCont(new NestedModule(m))
         case _ => throw ImplementationError("bad parent")
      }
   } 
  /** auxiliary function to read Theories */
  private def readTheory(parent: Path)(implicit state: ParserState) {
      val rname = readName
      val (ns,name) = parent match {
         case doc: DPath =>
            val ns = DPath(state.namespaces.default)
            val mref = MRef(doc, ns ? rname, true)
            seCont(mref)
            (ns, rname)
         case mp: MPath =>
            (mp.doc, mp.name / rname)
         case _ => throw ImplementationError("bad parent")
      }
      val tpath = ns ? name
      var delim = state.reader.readToken
      if (delim._1 == "abbrev") {
         val (_,_,df) = readParsedObject(OMMOD(tpath))
         val thy = DefinedTheory(ns, name, df)
         moduleCont(thy, parent)
      } else {
         val meta = if (delim._1 == ":") {
            val p = readMPath(tpath)
            delim = state.reader.readToken
            Some(p)
         } else
            None
         val t = new DeclaredTheory(ns, name, meta)
         moduleCont(t, parent)
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
                     errorCont(makeError(delim._2, "error while retrieving patterns, continuing without patterns"))
                     Nil
                  }
            }
            logGroup {
               readInModule(t, patterns)
            }
            end(t)
         } else {
            throw makeError(delim._2, "':' or '=' or 'abbrev' expected")
         }
      }
   }
   /** auxiliary function to unify "view" and "implicit view" */
   private def readView(parent: Path, isImplicit: Boolean)(implicit state: ParserState) {
      val rname = readName
      val (ns,name) = parent match {
         case doc: DPath =>
            val ns = DPath(state.namespaces.default)
            val mref = MRef(doc, ns ? rname, true)
            seCont(mref)
            (ns, rname)
         case mp: MPath =>
            (mp.doc, mp.name / rname)
         case _ => throw ImplementationError("bad parent")
      }
      val vpath = ns ? name
      readDelimiter(":")
      val from = readMPath(vpath)
      readDelimiter("->","→")
      val to = readMPath(vpath)
      readDelimiter("abbrev", "=") match {
         case "abbrev" =>
            val (_,_,df) = readParsedObject(OMMOD(vpath))
            val v = DefinedView(ns, name, OMMOD(from), OMMOD(to), df, isImplicit)
            moduleCont(v, parent)
         case "=" =>
            val v = new DeclaredView(ns, name, OMMOD(from), OMMOD(to), isImplicit)
            moduleCont(v, parent)
            logGroup {
               readInModule(v, Nil)
            }
            end(v)
      }
   }
   /** the main loop for reading declarations that can occur in documents
    * @param doc the containing Document (must be in the controller already)
    */
   private def readInDocument(doc: Document)(implicit state: ParserState) {
      if (state.reader.endOfDocument) return
      val (keyword, reg) = state.reader.readToken
      state.startPosition = reg.start
      try {
         keyword match {
            case "" =>
               if (state.reader.endOfDocument) {
                  return
               } else
                  throw makeError(reg, "keyword expected, within " + doc).copy(fatal = true)
            case "document" =>
               val name = readName
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
               // default namespace is set relative to current default namespace
               val ns = readDPath(DPath(state.namespaces.default))
               state.namespaces.default = ns.uri
            case "import" =>
               val (n,_) = state.reader.readToken
               val ns = readDPath(DPath(state.namespaces.default))
               state.namespaces.prefixes(n) = ns.uri
            case "theory" =>
               readTheory(doc.path)
            case "view" | "morphism" => readView(doc.path, false)
            case "implicit" =>
               val (keyword2, reg2) = state.reader.readToken
               keyword2 match {
                  case "view" | "morphism" => readView(doc.path, true)
                  case _ => throw makeError(reg2, "only views can be implicit here")
               }
            case k =>
               // other keywords are treated as parser plugins
               val extParser = controller.extman.getParserExtension(doc, k).getOrElse {
                  throw makeError(reg, "unknown keyword: " + k)
               }
               val (mod, mreg) = state.reader.readModule
               val reader = Reader(mod)
               reader.setSourcePosition(mreg.start)
               extParser(this, state.copy(reader), doc, k)
         }
         // check that the reader is at the end of a module level declaration, throws error otherwise
         if (! state.reader.endOfModule) {
            val (rest, reg) = state.reader.readModule
            if (rest != "")
               throw makeError(reg, "extraneous tokens, ignored: " + rest)
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
   private def readInModule(mod: DeclaredModule, patterns: List[(String,GlobalName)])(implicit state: ParserState) {
      //This would make the last RS marker of a module optional, but it's problematic with nested modules.
      //if (state.reader.endOfModule) return
      val scope = mod match {
         case t: DeclaredTheory => OMMOD(t.path)
         case l: DeclaredLink => l.to 
      }
      try {
         val (keyword, reg) = state.reader.readToken
         if (keyword == "kind")
            true
         state.startPosition = reg.start
         def fail(s: String) = throw makeError(reg,s)
         keyword match {
            //this case occurs if we read the GS marker
            case "" =>
               if (state.reader.endOfModule) {
                  return
               } else
                  fail("keyword expected, within module " + mod.path)
            //Constant
            case "constant" =>
               val name = readName
               val c = readConstant(name, mod.path, scope)
               seCont(c)
            //PlainInclude
            case "include" =>
               mod match {
                  case thy: DeclaredTheory =>
                     val from = readMPath(thy.path)
                     val incl = PlainInclude(from, thy.path)
                     seCont(incl)
                  case link: DeclaredLink =>
                     val from = readMPath(link.path)
                     readDelimiter("=")
                     val incl = readMPath(link.path) //readParsedObject(view.to)
                     val as = PlainViewInclude(link.toTerm, from, incl)
                     seCont(as)
               }
            case "theory" => readTheory(mod.path)
            //Pattern
            case "pattern" =>
              mod match {
                  case thy: DeclaredTheory =>
                     val name = readName
                     thy.meta match {
                        case None =>
                           fail("pattern declaration illegal without meta-theory")
                        case Some(mt) =>
//                           val pattern = readSPath(mt)
                           readPattern(name: LocalName, thy.path)
                     }
                  case link: DeclaredLink =>
                     fail("pattern declaration in link")
               }
            //Instance
            case "instance" =>
               mod match {
                  case thy: DeclaredTheory =>
                     val name = readName
                     thy.meta match {
                        case None =>
                           fail("instance declaration illegal without meta-theory")
                        case Some(mt) =>
                           readInstance(name, thy.path, None)
                     }
                  case link: DeclaredLink =>
                     fail("instance declaration in link")
               }
            case k =>
               // other keywords are treated as ...
               val patOpt = patterns.find(_._1 == k)
               if (patOpt.isDefined) {
                  // 1) an instance of a Pattern with LocalName k visible in meta-theory 
                  val pattern = patOpt.get._2
                  val name = readName
                  readInstance(name, mod.path, Some(pattern))
               } else {
                  val parsOpt = controller.extman.getParserExtension(mod, k)
                  if (parsOpt.isDefined) {
                     // 2) a parser plugin identified by k
                     val (decl, reg) = state.reader.readDeclaration
                     val reader = Reader(decl)
                     reader.setSourcePosition(reg.start)
                     parsOpt.get.apply(this, state.copy(reader), mod, k)
                  } else {
                     // 3) a constant with name k
                     val name = LocalName.parse(k)
                     val c = readConstant(name, mod.path, scope)
                     seCont(c)
                  }
               }
         }
         if (! state.reader.endOfDeclaration) {
            val (rest, reg) = state.reader.readDeclaration
            if (rest != "")
               throw makeError(reg, "end of declaration expected, found and ignored: " + rest)
         }
      } catch {
         case e: SourceError =>
            errorCont(e)
            if (! state.reader.endOfDeclaration)
               state.reader.readDeclaration
      }
      readInModule(mod, patterns) // compiled code is not actually tail-recursive
   }
   
   private def doComponent(c: DeclarationComponent, tc: TermContainer, scope: Term, cont: Context = Context())(implicit state: ParserState) {
         val (obj,_,tm) = readParsedObject(scope, cont)
         tc.read = obj
         tc.parsed = tm
      }
   private def doNotation(c: NotationComponent, nc: NotationContainer, treg: SourceRegion, cpath: GlobalName)(implicit state: ParserState) {
         val (notString,reg) = state.reader.readObject
         if (nc(c).isDefined)
            errorCont(makeError(treg, "notation of this constant already given, ignored"))
         else {
            val notation = TextNotation.parse(notString, cpath)
            nc(c) = notation
         }
      }
   /** reads the components of a [[info.kwarc.mmt.api.symbols.Constant]]
    * @param name the name of the constant
    * @param parent the containing [[info.kwarc.mmt.api.modules.DeclaredModule]]
    * @param scope the home theory for term components
    */
   private def readConstant(name: LocalName, parent: MPath, scope: Term)(implicit state: ParserState): Constant = {
      val cpath = parent ? name
      //initialize all components as omitted
      val tpC = new TermContainer
      val dfC = new TermContainer
      var al : Option[LocalName] = None
      var nt = new NotationContainer
      var rl : Option[String] = None
      val cons = Constant(OMMOD(parent), name, None, tpC, dfC, None, nt)
      // every iteration reads one delimiter and one object
      // @ alias or : TYPE or = DEFINIENS or # NOTATION
      //TODO remove "##" here and in the case split below, only used temporarily for latex integration
      val keys = List(":","=","#", "##","@", "role")
      val keyString = keys.map("'" + _ + "'").mkString(", ")

      while (! state.reader.endOfDeclaration) {
         val (delim, treg) = state.reader.readToken
            // branch based on the delimiter
            delim match {
               case ":" =>
                  if (tpC.read.isDefined) {
                     errorCont(makeError(treg, "type of this constant already given, ignored"))
                     state.reader.readObject
                  } else
                     doComponent(TypeComponent, tpC, scope)
               case "=" =>
                  if (dfC.read.isDefined) {
                     errorCont(makeError(treg, "definiens of this constant already given, ignored"))
                     state.reader.readObject
                  } else
                     doComponent(DefComponent, dfC, scope)
               case "#" =>
                  doNotation(ParsingNotationComponent, nt, treg, cpath)
               case "##" =>
                  doNotation(PresentationNotationComponent, nt, treg, cpath)
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
               case k => controller.extman.getParserExtension(cons, k) match {
                  case Some(parser) =>
                     val (obj, reg) = state.reader.readObject
                     val reader = Reader(obj)
                     reader.setSourcePosition(reg.start)
                     parser(this, state.copy(reader), cons, k)
                  case None =>
                     if (! state.reader.endOfDeclaration) {
                        errorCont(makeError(treg, "expected " + keyString + ", found " + k))
                     } else if (k != "") { 
                        if (! state.reader.endOfObject)
                           state.reader.readObject
                        errorCont(makeError(treg, "expected " + keyString + ", ignoring the next object"))
                     }
            }
         }
      }
      val constant = Constant(OMMOD(parent), name, al, tpC, dfC, rl, nt)
      constant.metadata = cons.metadata
      constant
   }
   private def readInstance(name: LocalName, tpath: MPath, patOpt: Option[GlobalName])(implicit state: ParserState) {
      var pattern: GlobalName = null
      var arguments: List[Term] = Nil
      if (state.reader.endOfDeclaration) {
         patOpt match {
            case Some(p) => pattern = p
            case None => throw makeError(state.reader.getSourcePosition.toRegion, "instance declaration expected")
         }
      } else { 
         val (obj,reg,tm) = readParsedObject(OMMOD(tpath))
         controller.pragmatic.mostPragmatic(tm) match {
            case OMA(OMS(pat), args) =>
               pattern = pat
               arguments = args
            case OMS(pat) =>
               pattern = pat
            case _ =>
              val patString = patOpt match {
                case None => "of a pattern"
                case Some(p) => "of pattern " + p.toPath
              }
              throw makeError(reg, "not an instance of pattern " + patString)
         }
	     patOpt foreach {p =>
	         if (p != pattern)
	            throw makeError(reg, "not an instance of pattern " + p.toPath)
	     }
      }
      val instance = new Instance(OMMOD(tpath), name, pattern, arguments)
      seCont(instance)
   }
   private def readPattern(name: LocalName, tpath: MPath)(implicit state: ParserState) {
      val ppath = tpath ? name
      val tpC = new TermContainer
      val dfC = new TermContainer
      var nt = new NotationContainer
      var pr : Context = Context()// params
      var bd : Context = Context()// body
      while (! state.reader.endOfDeclaration) {
        val (delim, treg) = state.reader.readToken
        // branch based on the delimiter
        delim match {
          case "::" =>
            val (obj, reg) = state.reader.readObject
            val pu = ParsingUnit(SourceRef(state.container.uri, reg), OMMOD(tpath), Context(), obj, None)
            val parsed = puCont(pu)
            parsed match {
              case OMBINDC(_, cont, Nil) =>
                pr = pr ++ cont
              case _ =>
                errorCont(makeError(reg, "parameters of this constant are not a context, ignored (note that implicit parts are not allowed in parameters)"))
            }
          case ">>" =>
            val (obj, reg) = state.reader.readObject
            // keep parameters in the context
            val pu = ParsingUnit(SourceRef(state.container.uri, reg), OMMOD(tpath), pr, obj, None)
            val parsed = puCont(pu)
            parsed match {
              case OMBINDC(_, cont, Nil) =>
                bd = bd ++ cont
              case _ =>
                errorCont(makeError(reg, "parameters of this constant are not a context, ignored (note that implicit parts are not allowed in parameters)"))
            }
          case "#" =>
             doNotation(ParsingNotationComponent, nt, treg, ppath)
          case "##" =>
             doNotation(PresentationNotationComponent, nt, treg, ppath)
        }
      }
      val pattern = new Pattern(OMMOD(tpath),name, pr, bd, nt)
      seCont(pattern)
   }
   
   /** API Functions for parsing fragments, TODO refactor **/
   		   			
   /**
    * reads a module from a string
    * @param dpath the uri of the parent document
    * @param modS the module string
    * @param docBaseO optionally the base uri of the containing document 
    * @param namespace the namespace declarations found so far
    */
   def readModule(dpath : DPath, modS : String, docBaseO : Option[DPath] = None,
          namespace : ListMap[String, DPath] = new ListMap, errorCont: ErrorHandler) {
     //building parsing state
     val r = new Reader(new java.io.BufferedReader(new java.io.StringReader(modS)))
     val state = new ParserState(r, docBaseO.getOrElse(null), errorCont)
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
   def readConstant(mpath : MPath, conS : String, docBaseO : Option[DPath] = None, 
                    namespace : ListMap[String, DPath] = new ListMap, errorCont: ErrorHandler) = {
     //building parsing state
     val r = new Reader(new java.io.BufferedReader(new java.io.StringReader(conS)))
     val state = new ParserState(r, docBaseO.getOrElse(null), errorCont)
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
     readInModule(thy, patterns)(state)
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



