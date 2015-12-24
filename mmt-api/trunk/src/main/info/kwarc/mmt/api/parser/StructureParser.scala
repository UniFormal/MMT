package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import documents._
import modules._
import notations._
import objects._
import opaque._
import patterns._
import symbols._
import utils._

/**
 * This class bundles all state that is maintained by a [[StructureParser]]
 *
 * @param reader the input stream, from which the parser reads (see ps below)
 * @param ps the encapsulated input that contains the buffered reader (also encapsulated in reader!)
 */
class ParserState(val reader: Reader, val ps: ParsingStream, val errorCont: ErrorHandler) {
  /**
   * the namespace mapping set by
   * {{{
   * import prefix URI
   * }}}
   * commands
   *
   * the initial namespace map
   */
  var namespaces = ps.nsMap

  /** the position at which the current StructuralElement started */
  var startPosition = reader.getSourcePosition

  def copy(rd: Reader = reader): ParserState = {
    val s = new ParserState(rd, ps, errorCont)
    s.namespaces = namespaces
    s
  }
  
  def makeSourceRef(reg: SourceRegion) = SourceRef(ps.source, reg)
}

/** matches the keyword for a view */
object ViewKey {
  def unapply(s: String): Option[String] = s match {
    case "view" | "morphism" => Some(s)
    case _ => None
  }
}

/**
 * only adjusted resolveName to avoid reading content
 */
class RelKeywordBasedParser extends KeywordBasedParser(DefaultObjectParser) {
  override def resolveName(home: Term, name: LocalName)(implicit state: ParserState) = name

  override def getPatternsFromMeta(_o: Option[MPath]): List[(String, GlobalName)] = Nil

  override def seCont(se: StructuralElement)(implicit state: ParserState) =
    se match {
      case _: Constant =>
      case _ =>
        val reg = currentSourceRegion
        SourceRef.update(se, state.makeSourceRef(reg))
        try {
           controller.memory.content.add(se)
        } catch {
          case e: Error => log("error after parsing: " + e.getMessage)
        }
    }

  override def getParseExt(se: StructuralElement, key: String): Option[ParserExtension] =
    key match {
      case "rule" => Some(RelRuleParser)
      case _ =>
        super.getParseExt(se, key)
    }

}

object RelRuleParser extends RuleConstantParser {
  override def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String, con:Context = Context.empty) =
    s.reader.readDeclaration
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
 * throw [[SourceError]] where appropriate.
 *
 * 2) It is stateless and maintains the parse state via an implicit argument of type
 * [[ParserState]] in most functions.
 *
 * 3) It leaves processing of MMT entities application-independently via high-level continuation functions.
 */

// TODO first add all structure, notations, then check semantics

class KeywordBasedParser(objectParser: ObjectParser) extends Parser(objectParser) {
  override val logPrefix = "structure-parser"
  val format = "mmt"

  // *********************************** interface to the controller: add elements and errors etc.
  
  /**
   * A continuation function called on every StructuralElement that was found
   *
   * For grouping elements (documents, modules with body), this must be called on the empty element first
   * and then on each child, finally end(se) must be called on the grouping element.
   */
  protected def seCont(se: StructuralElement)(implicit state: ParserState) {
    log(se.toString)
    val reg = currentSourceRegion
    SourceRef.update(se, state.makeSourceRef(reg))
    try {
      controller.add(se)
    }
    catch {
      case e: Error =>
        val se = makeError(reg, "after parsing: " + e.getMessage)
        se.setCausedBy(e)
        errorCont(se)
    }
  }
  /** called at the end of a document or module, does common bureaucracy */
  protected def end(s: StructuralElement)(implicit state: ParserState) {
    //extend source reference until end of element
    SourceRef.get(s) foreach { r =>
      SourceRef.update(s, r.copy(region = r.region.copy(end = state.reader.getSourcePosition)))
    }
    log("end " + s.path)
  }
  /** the region from the start of the current structural element to the current position */
  protected def currentSourceRegion(implicit state: ParserState) =
    SourceRegion(state.startPosition, state.reader.getSourcePosition)


  /** like seCont but may wrap in NestedModule */
  private def moduleCont(m: Module, par: ParentInfo)(implicit state: ParserState) {
    val se = par match {
       case IsDoc(dp) => m
       case IsMod(mp, ln) =>
          val nm = new NestedModule(m)
          nm.setDocumentHome(ln)
          nm
    }
    seCont(se)
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
        val se = makeError(pu.source.region, "during parsing: " + e.getMessage)
        se.setCausedBy(e)
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
  /** convenience function to create SourceError's */
  protected def makeError(reg: SourceRegion, s: String)(implicit state: ParserState) =
    SourceError("structure-parser", state.makeSourceRef(reg), s)


  // ******************************* the entry points
  
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): Document = {
    val (d, _) = apply(new ParserState(new Reader(ps.stream), ps, errorCont))
    d
  }

  private def apply(state: ParserState): (Document, ParserState) = {
    val dpath = state.ps.dpath
    val doc = new Document(dpath, root = true, nsMap = state.namespaces)
    seCont(doc)(state)
    logGroup {
      readInDocument(doc)(state)
    }
    end(doc)(state)
    (doc, state)
  }

  // ********************************* low level read functions for names, terms, etc.
  
  /** read a LocalName from the stream
    * throws SourceError iff ill-formed or empty
    */
  def readName(implicit state: ParserState): LocalName = {
    val (s, reg) = state.reader.readToSpace
    if (s == "")
      throw makeError(reg, "name expected")
    try {
      LocalName.parse(s)
    }
    catch {
      case e: ParseError =>
        throw makeError(reg, "invalid identifier: " + e.getMessage)
    }
  }

  /** read a DPath from the stream
    * throws SourceError iff ill-formed or empty
    */
  def readDPath(newBase: Path)(implicit state: ParserState): DPath = {
    val (s, reg) = state.reader.readToSpace
    if (s == "")
      throw makeError(reg, "MMT URI expected")
    try {
      Path.parseD(s, state.namespaces(newBase))
    }
    catch {
      case e: ParseError =>
        throw makeError(reg, "invalid identifier: " + e.getMessage)
    }
  }

  /** read a MPath from the stream
    * throws SourceError iff ill-formed or empty
    */
  def readMPath(newBase: Path)(implicit state: ParserState): (SourceRef, MPath) = {
    val (s, reg) = state.reader.readToSpace
    if (s == "")
      throw makeError(reg, "MMT URI expected")
    val mp = try {
      Path.parseM(s, state.namespaces(newBase))
    }
    catch {
      case e: ParseError =>
        throw makeError(reg, "invalid identifier: " + e.getMessage)
    }
    val ref = state.makeSourceRef(reg)
    (ref, mp)
  }

  /** read a GlobalName from the stream
    * throws SourceError iff ill-formed or empty
    */
  def readSPath(newBase: Path)(implicit state: ParserState): GlobalName = {
    val (s, reg) = state.reader.readToSpace
    if (s == "")
      throw makeError(reg, "MMT URI expected")
    try {
      Path.parseS(s, state.namespaces(newBase))
    }
    catch {
      case e: ParseError =>
        throw makeError(reg, "invalid identifier: " + e.getMessage)
    }
  }

  /**
   * reads one out of a list of permitted delimiters
   * @param delims the permitted delimiter
   * @return the read delimiter
   * throws SourceError iff anything else found
   */
  def readDelimiter(delims: String*)(implicit state: ParserState): String = {
    val delim = state.reader.readToken
    if (delims.contains(delim._1))
      delim._1
    else
      throw makeError(delim._2, delims.map("'" + _ + "'").mkString("", " or ", "") + "expected")
  }

  /**
   * reads until the object delimiter and parses the found string
   * @return the raw string, the region, and the parsed term
   */
  def readParsedObject(context: Context, topRule: Option[ParsingRule] = None)(implicit state: ParserState): (String, SourceRegion, Term) = {
    val (obj, reg) = state.reader.readObject
    val pu = ParsingUnit(state.makeSourceRef(reg), context, obj, state.namespaces, topRule)
    val parsed = puCont(pu)
    (obj, reg, parsed)
  }

  private def doComponent(c: ComponentKey, tc: TermContainer, cont: Context)(implicit state: ParserState) {
    val (obj, _, tm) = readParsedObject(cont)
    tc.read = obj
    tc.parsed = tm
  }

  private def doNotation(c: NotationComponentKey, nc: NotationContainer, treg: SourceRegion,
                         cpath: GlobalName)(implicit state: ParserState) {
    val notString = state.reader.readObject._1
    if (nc(c).isDefined)
      errorCont(makeError(treg, "notation of this constant already given, ignored"))
    else {
      val notation = TextNotation.parse(notString, state.namespaces(cpath))
      nc(c) = notation
    }
  }

  protected def resolveName(home: Term, name: LocalName)(implicit state: ParserState) = {
    libraries.Names.resolve(home, name)(controller.globalLookup) match {
      case Some(ce: Constant) =>
        ComplexStep(ce.parent) / ce.name
      case Some(_) =>
        errorCont(makeError(currentSourceRegion, "not a constant name: " + name))
        name
      case None =>
        errorCont(makeError(currentSourceRegion, "unknown name: " + name))
        name
    }
  }
  
  // *************** the two major methods for reading in documents and modules 

  /** abstraction to unify operations inside a document and inside a module */ 
  private abstract class ParentInfo {
     def docHome: DPath
  }
  private case class IsDoc(docHome: DPath) extends ParentInfo
  private case class IsMod(mod: MPath, relDoc: LocalName) extends ParentInfo {
     def docHome = mod.toDPath / relDoc
  }  
  
  /** the main loop for reading declarations that can occur in documents
    * @param doc the containing Document (must be in the controller already)
    */
  private def readInDocument(doc: Document)(implicit state: ParserState) {
    if (state.reader.endOfDocument) return
    val (keyword, reg) = state.reader.readToken
    val parentInfo = IsDoc(doc.path)
    state.startPosition = reg.start
    try {
      keyword match {
        case "" =>
          if (state.reader.endOfDocument) {
            return
          } else
            throw makeError(reg, "keyword expected, within " + doc).copy(level = Level.Fatal)
        case "document" =>
          val name = readName
          val dpath = doc.path / name
          val d = new Document(dpath)
          seCont(d)
          logGroup {
            readInDocument(d)
          }
          end(d)
        case "/" =>
            val oe = readOpaque(parentInfo, Context.empty)
            seCont(oe)
        case "namespace" =>
          // default namespace is set relative to current default namespace
          val ns = readDPath(DPath(state.namespaces.default))
          state.namespaces = state.namespaces(DPath(ns.uri))
        case "import" =>
          val (n, _) = state.reader.readToken
          val ns = readDPath(DPath(state.namespaces.default))
          state.namespaces = state.namespaces.add(n, ns.uri)
        case "theory" =>
          readTheory(parentInfo, Context.empty)
        case ViewKey(_) => readView(parentInfo, Context.empty, isImplicit = false)
        case "implicit" =>
          val (keyword2, reg2) = state.reader.readToken
          keyword2 match {
            case ViewKey(_) => readView(parentInfo, Context.empty, isImplicit = true)
            case _ => throw makeError(reg2, "only views can be implicit here")
          }
        case k =>
          // other keywords are treated as parser plugins
          val extParser = getParseExt(doc, k).getOrElse {
            throw makeError(reg, "unknown keyword: " + k)
          }
          val (mod, mreg) = state.reader.readModule
          val reader = Reader(mod)
          reader.setSourcePosition(mreg.start)
          extParser(this, state.copy(reader), doc, k)
      }
      // check that the reader is at the end of a module level declaration, throws error otherwise
      if (!state.reader.endOfModule) {
        val (rest, reg) = state.reader.readModule
        if (rest != "")
          throw makeError(reg, "extraneous tokens, ignored: " + rest)
      }
    } catch {
      case e: SourceError =>
        errorCont(e)
        if (!state.reader.endOfModule)
          state.reader.readModule
    }
    readInDocument(doc) // compiled code is not actually tail-recursive
  }

  /** the main loop for reading declarations that can occur in a theory
    * @param mod the containing module (added already)
    * @param context the context (including the containing module)
    * @param patterns the patterns of the meta-theory (precomputed in readInDocument)
    *
    *                 this function handles one declaration if possible, then calls itself recursively
    */
  private def readInModule(mod: Body, context: Context,
                           patterns: List[(String, GlobalName)])(implicit state: ParserState) {
     readInModuleAux(mod, mod.asDocument.path, context, patterns)
  }
  
  private def readInModuleAux(mod: Body, docHome: DPath, context: Context,
                           patterns: List[(String, GlobalName)])(implicit state: ParserState) {
    //This would make the last RS marker of a module optional, but it's problematic with nested modules.
    //if (state.reader.endOfModule) return
    val linkOpt = mod match {
      case l: DeclaredLink => Some(l)
      case _ => None
    }
    val mpath = mod.path.toMPath // non-trivial only for structures
    /** the root name of all sections and the LocalName of the currentSection */
    val docRoot = mpath.toDPath
    val currentSection = docHome.dropPrefix(docRoot).getOrElse {
       throw ImplementationError("document home must extend content home")
    }
    lazy val parentInfo = IsMod(mpath, currentSection)
    /* declarations must only be added through this method */
    def addDeclaration(d: Declaration) {
         d.setDocumentHome(currentSection)
         seCont(d)
    }
    // to be set if the section changes
    var nextSection = currentSection
    try {
      val (keyword, reg) = state.reader.readToken
      state.startPosition = reg.start
      def fail(s: String) = throw makeError(reg, s)
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
          val c = readConstant(name, mpath, linkOpt, context)
          addDeclaration(c)
        //PlainInclude
        case "include" =>
          mod match {
            case thy: DeclaredTheory =>
              val (fromRef, from) = readMPath(thy.path)
              val incl = PlainInclude(from, thy.path)
              SourceRef.update(incl.from, fromRef)
              addDeclaration(incl)
            case link: DeclaredLink =>
              val (fromRef, from) = readMPath(link.path)
              readDelimiter("=")
              val (inclRef, incl) = readMPath(link.path)
              //readParsedObject(view.to)
              val as = PlainViewInclude(link.toTerm, from, incl)
              SourceRef.update(as.from, fromRef)
              SourceRef.update(as.df, inclRef)
              addDeclaration(as)
          }
        case "structure" =>
          mod match {
            case thy: DeclaredTheory => readStructure(parentInfo, linkOpt, context, isImplicit = false)
            case link: DeclaredLink =>
              val name = readName
              val orig = controller.get(link.from.toMPath ? name) match {
                case s: Structure => s
                case _ => fail("not a structure: "+link.from.toMPath?name)
              }
              readDelimiter("=")
              val incl = readSPath(link.path)
              val target = controller.get(incl) match {
                case s: Link => s
                case _ => fail("not a link: "+incl)
              }
              val as = DefLinkAssignment(link.toTerm,name,target.from,target.toTerm)
              // SourceRef.update(as.df,SourceRef.fromURI(URI.apply(target.path.toString)))
              // SourceRef.update(as.from,SourceRef.fromURI(URI.apply(orig.path.toString)))
              addDeclaration(as)
          }
        case "theory" => readTheory(parentInfo, context)
        case ViewKey(_) => readView(parentInfo, context, isImplicit = false)
        case k if k.forall(_ == '=') =>
            val currentLevel = currentSection.length
            val thisLevel = k.length
            val (nameTitle,reg) = state.reader.readDeclaration
            // close all sections from currentLevel up to and including thisLevel
            if (thisLevel <= currentLevel) {
               Range(currentLevel,thisLevel-1,-1).foreach {l =>
                  mod.asDocument.getLocally(currentSection.take(l)).foreach {d =>
                    end(d)
                  }
               }
               nextSection = currentSection.take(thisLevel-1)
            }
            if (nameTitle.isEmpty) {
               // just end a section: =...= 
               if (thisLevel > currentLevel) {
                  throw makeError(reg, s"no document at level $thisLevel open")
               }
            } else {
               // additionally begin a new section: =...= #NAME TITLE  or =...= TITLE
               if (thisLevel > currentLevel+1) {
                  throw makeError(reg, s"no document at level ${thisLevel-1} open")
               }
               val (name,title) = if (nameTitle.startsWith("#")) {
                  val pos = nameTitle.indexWhere(_.isWhitespace)
                  (nameTitle.substring(1,pos),nameTitle.substring(pos).trim)
               } else ("", nameTitle)
               nextSection = nextSection / name
               val innerDoc = new Document(docRoot / nextSection, contentAncestor = Some(mod))
               NarrativeMetadata.title.update(innerDoc, title)
               seCont(innerDoc)
            }
        case "/" =>
            val oe = readOpaque(parentInfo, context)
            seCont(oe)
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
                  val p = readPattern(name: LocalName, thy.path)
                  addDeclaration(p)
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
                  val i = readInstance(name, thy.path, None)
                  addDeclaration(i)
              }
            case link: DeclaredLink =>
              fail("instance declaration in link")
          }
        case "implicit" =>
          val (keyword2, reg2) = state.reader.readToken
          keyword2 match {
            case ViewKey(_) => readView(parentInfo, context, isImplicit = true)
            case "structure" => readStructure(parentInfo, linkOpt, context, isImplicit = true)
            case _ => throw makeError(reg2, "only links can be implicit here")
          }
        case k =>
          // other keywords are treated as ...
          val patOpt = patterns.find(_._1 == k)
          if (patOpt.isDefined) {
            // 1) an instance of a Pattern with LocalName k visible in meta-theory
            val pattern = patOpt.get._2
            val name = readName
            val i = readInstance(name, mpath, Some(pattern))
            addDeclaration(i)
          } else {
            val parsOpt = getParseExt(mod, k)
            if (parsOpt.isDefined) {
              // 2) a parser plugin identified by k
              val (decl, reg) = state.reader.readDeclaration
              val reader = Reader(decl)
              reader.setSourcePosition(reg.start)
              val se = if (currentSection.length == 0) mod
              else mod.asDocument.getLocally(currentSection).getOrElse {
                 throw ImplementationError("section not found in module")
              }
              parsOpt.get.apply(this, state.copy(reader), se, k,context)
            } else {
              // 3) a constant with name k
              val name = LocalName.parse(k)
              val c = readConstant(name, mpath, linkOpt, context)
              addDeclaration(c)
            }
          }
      }
      if (!state.reader.endOfDeclaration) {
        val (rest, reg) = state.reader.readDeclaration
        if (rest != "")
          throw makeError(reg, "end of declaration expected, found and ignored: " + rest)
      }
    } catch {
      case e: Error =>
        // wrap in source error if not source error already
        val se: SourceError = e match {
          case se: SourceError => se
          case _ => makeError(currentSourceRegion, "error while parsing: " + e.getMessage).setCausedBy(e)
        }
        errorCont(se)
        if (!state.reader.endOfDeclaration)
          state.reader.readDeclaration
    }
    readInModuleAux(mod, docRoot / nextSection, context, patterns) // compiled code is not actually tail-recursive
  }

  // *************** auxiliary methods of readInModule and readInDocument that read particular elements 
  
  private def readParameters(context: Context)(implicit state: ParserState): Context = {
    val (name,srg) = state.reader.readToken
    readDelimiter(":")
    val (obj,reg,parsed) = readParsedObject(context)
    val vdc = VarDecl(LocalName(name),Some(parsed),None,None)
    if (!state.reader.endOfDeclaration) readParameters(context++vdc)++vdc
      else vdc
  }
  /** auxiliary function to read Theories
    * @param docHome the containing document (not the namespace) or module
    * @param modHome the containing theory (if any)
    * @param context the context (excluding the theory to be read)
    */
  private def readTheory(parent: ParentInfo, context: Context)(implicit state: ParserState) {
    val rname = readName
    val (ns, name) = parent match {
      case IsDoc(doc) =>
        val ns = DPath(state.namespaces.default)
        val mref = MRef(doc, ns ? rname)
        seCont(mref)
        (ns, rname)
      case IsMod(mod,_) =>
        (mod.doc, mod.name / rname)
    }
    val tpath = ns ? name
    var delim = state.reader.readToken
    if (delim._1 == "abbrev") {
      val (_, _, df) = readParsedObject(context)
      val thy = DefinedTheory(ns, name, df)
      moduleCont(thy, parent)
    } else {
      val meta = if (delim._1 == ":") {
        val (_, p) = readMPath(tpath)
        delim = state.reader.readToken
        Some(p)
      } else
        None
      val contextMeta = meta match {
        case Some(p) => context ++ p
        case _ => context
      }

      val contextRule = ParsingRule(utils.mmt.context, TextNotation(Precedence.integer(0), None)(Var(1, true, Some(Delim(",")))))
      val parameters = if (delim._1 == ">") {
        val (_, reg, p) = readParsedObject(contextMeta, Some(contextRule))
        val params = ObjectParser.splitOffUnknowns(p) match {
          case (unk, OMBINDC(OMS(utils.mmt.context), cont, Nil)) => unk ++ cont
          case _ =>
            errorCont(makeError(reg, "parameters of theory must be context: " + controller.presenter.asString(p)))
            Context.empty
        }
        delim = state.reader.readToken
        params

        /*
        val ct = readParameters(contextMeta)
        delim = state.reader.readToken
        ct
        */
      } else
        Context.empty
      val t = new DeclaredTheory(ns, name, meta, parameters)
      moduleCont(t, parent)
      if (delim._1 == "=") {
        val patterns: List[(String, GlobalName)] = getPatternsFromMeta(meta)
        logGroup {
          readInModule(t, context ++ t.getInnerContext, patterns)
        }
        end(t)
      } else {
        throw makeError(delim._2, "':' or '=' or 'abbrev' expected")
      }
    }
  }

  protected def getPatternsFromMeta(meta: Option[MPath]): List[(String, GlobalName)] =
    meta match {
      case None => Nil
      case Some(mt) =>
        try {
          //TODO this does not cover imported patterns
          controller.globalLookup.getDeclaredTheory(mt).getPatterns.map {
            p => (p.name.toPath, p.path)
          }
        } catch {
          case e: Error =>
            //errorCont(makeError(delim._2, "error while retrieving patterns, continuing without patterns"))
            Nil
        }
    }

  /** auxiliary function to read views
    * @param parent the containing document/module
    * @param context the context (excluding the view to be read)
    * @param isImplicit whether the view is implicit
    */
  private def readView(parent: ParentInfo, context: Context, isImplicit: Boolean)(implicit state: ParserState) {
    val rname = readName
    val (ns, name) = parent match {
      case IsDoc(doc) =>
        val ns = DPath(state.namespaces.default)
        val mref = MRef(doc, ns ? rname)
        seCont(mref)
        (ns, rname)
      case IsMod(mod,_) =>
        (mod.doc, mod.name / rname)
    }
    val vpath = ns ? name
    readDelimiter(":")
    val (fromRef, fromPath) = readMPath(vpath)
    val from = OMMOD(fromPath)
    SourceRef.update(from, fromRef)
    readDelimiter("->", "→")
    val (toRef, toPath) = readMPath(vpath)
    val to = OMMOD(toPath)
    SourceRef.update(to, toRef)
    readDelimiter("abbrev", "=") match {
      case "abbrev" =>
        val (_, _, df) = readParsedObject(context)
        val v = DefinedView(ns, name, from, to, df, isImplicit)
        moduleCont(v, parent)
      case "=" =>
        val v = new DeclaredView(ns, name, from, to, isImplicit) // TODO add metamorph?
        moduleCont(v, parent)
        logGroup {
          readInModule(v, context ++ v.codomainAsContext, Nil)
        }
        end(v)
    }
  }

  /**
   * allow to control certain parser extensions
   * i.e. those with side effects like [[RuleConstantParser]]
   */
  protected def getParseExt(se: StructuralElement, key: String): Option[ParserExtension] =
    controller.extman.getParserExtension(se, key)

  /** reads the components of a [[Constant]]
    * @param givenName the name of the constant
    * @param parent the containing [[DeclaredModule]]
    * @param link the home theory for term components
    */
  private def readConstant(givenName: LocalName, parent: MPath, link: Option[DeclaredLink],
                           context: Context)(implicit state: ParserState): Constant = {
    val name = link.map { l => resolveName(l.from, givenName) }.getOrElse(givenName)
    val cpath = parent ? name
    //initialize all components as omitted
    val tpC = new TermContainer
    val dfC = new TermContainer
    var al: Option[LocalName] = None
    val nt = new NotationContainer
    var rl: Option[String] = None
    val cons = Constant(OMMOD(parent), name, None, tpC, dfC, None, nt)
    // every iteration reads one delimiter and one object
    // @ alias or : TYPE or = DEFINIENS or #(#) NOTATION
    val keys = List(":", "=", "#", "##", "@", "role")
    val keyString = keys.map("'" + _ + "'").mkString(", ")

    while (!state.reader.endOfDeclaration) {
      val (delim, treg) = state.reader.readToken
      // branch based on the delimiter
      delim match {
        case ":" =>
          if (tpC.read.isDefined) {
            errorCont(makeError(treg, "type of this constant already given, ignored"))
            state.reader.readObject
          } else
            doComponent(TypeComponent, tpC, context)
        case "=" =>
          if (dfC.read.isDefined) {
            errorCont(makeError(treg, "definiens of this constant already given, ignored"))
            state.reader.readObject
          } else
            doComponent(DefComponent, dfC, context)
        case "#" =>
          doNotation(ParsingNotationComponent, nt, treg, cpath)
        case "##" =>
          doNotation(PresentationNotationComponent, nt, treg, cpath)
        case "@" =>
          val (str, _) = state.reader.readObject
          if (al.isDefined)
            errorCont(makeError(treg, "alias of this constant already given, ignored"))
          else {
            al = Some(LocalName.parse(str))
          }
        case "role" =>
          val (str, _) = state.reader.readObject
          rl = Some(str)
        case k => getParseExt(cons, k) match {
          case Some(parser) =>
            val (obj, reg) = state.reader.readObject
            val reader = Reader(obj)
            reader.setSourcePosition(reg.start)
            parser(this, state.copy(reader), cons, k, context)
          case None =>
            if (!state.reader.endOfDeclaration) {
              errorCont(makeError(treg, "expected " + keyString + ", found " + k))
            } else if (k != "") {
              if (!state.reader.endOfObject)
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

  /** auxiliary function to read structures
    * @param parent the containing module
    * @param context the context (excluding the structure to be read)
    * @param isImplicit whether the structure is implicit
    */
  private def readStructure(parent: IsMod, link: Option[DeclaredLink], context: Context,
                            isImplicit: Boolean)(implicit state: ParserState) {
    val givenName = readName
    val name = link.map { l => resolveName(l.from, givenName) }.getOrElse(givenName)
    val spath = parent.mod ? name
    readDelimiter(":")
    val tpC = new TermContainer
    //doComponent(TypeComponent, tpC, context)
    val tp = OMMOD(readMPath(spath)._2)
    tpC.parsed = tp
    val s = new DeclaredStructure(OMMOD(parent.mod), name, tpC, isImplicit) // TODO add metamorph?
    s.setDocumentHome(parent.relDoc)
    seCont(s)
    if (state.reader.endOfDeclaration) {
      // s: tp RS
      end(s)
      return
    }
    val (t, reg) = state.reader.readToken
    t match {
      case "=" =>
        // s : tp US = body GS
        logGroup {
          readInModule(s, context, Nil)
        }
      case "" =>
        // good: s : tp US RS
        // bad:  s : tp US _
        if (!state.reader.endOfDeclaration)
          throw makeError(reg, "'=' or end of declaration expected, within structure " + spath)
      case _ =>
        // s : tp US _
        throw makeError(reg, "'=' or end of declaration expected, within structure " + spath)
    }
    end(s)
  }

  private def readInstance(name: LocalName, tpath: MPath, patOpt: Option[GlobalName])(implicit state: ParserState): Instance = {
    var pattern: GlobalName = null
    var arguments: List[Term] = Nil
    if (state.reader.endOfDeclaration) {
      patOpt match {
        case Some(p) => pattern = p
        case None => throw makeError(state.reader.getSourcePosition.toRegion, "instance declaration expected")
      }
    } else {
      val (_, reg, tm) = readParsedObject(Context(tpath))
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
      patOpt foreach { p =>
        if (p != pattern)
          throw makeError(reg, "not an instance of pattern " + p.toPath)
      }
    }
    new Instance(OMMOD(tpath), name, pattern, arguments)
  }

  private def readPattern(name: LocalName, tpath: MPath)(implicit state: ParserState): Pattern = {
    val ppath = tpath ? name
    val nt = new NotationContainer
    var pr: Context = Context() // params
    var bd: Context = Context() // body
    while (!state.reader.endOfDeclaration) {
      val (delim, treg) = state.reader.readToken
      // branch based on the delimiter
      delim match {
        case "::" =>
          val (obj, reg) = state.reader.readObject
          val pu = ParsingUnit(state.makeSourceRef(reg), Context(tpath), obj, state.namespaces, None)
          val parsed = puCont(pu)
          parsed match {
            case OMBINDC(_, cont, Nil) =>
              pr = pr ++ cont
            case _ =>
              errorCont(makeError(reg, "parameters of this constant are not a context, ignored " +
                "(note that implicit parts are not allowed in parameters)"))
          }
        case ">>" =>
          val (obj, reg) = state.reader.readObject
          // keep parameters in the context
          val pu = ParsingUnit(state.makeSourceRef(reg), pr ++ tpath, obj, state.namespaces, None)
          val parsed = puCont(pu)
          parsed match {
            case OMBINDC(_, cont, Nil) =>
              bd = bd ++ cont
            case _ =>
              errorCont(makeError(reg,
                "parameters of this constant are not a context, ignored " +
                  "(note that implicit parts are not allowed in parameters)"))
          }
        case "#" =>
          doNotation(ParsingNotationComponent, nt, treg, ppath)
        case "##" =>
          doNotation(PresentationNotationComponent, nt, treg, ppath)
      }
    }
    new Pattern(OMMOD(tpath), name, pr, bd, nt)
  }

  def readOpaque(pi: ParentInfo, context: Context)(implicit state: ParserState): OpaqueElement = {
      val (format, freg) = state.reader.readToken
      val oi = controller.extman.get(classOf[OpaqueTextParser], format).getOrElse {
         throw makeError(freg, "unknown opaque format: " + format)
      }
      val (text, treg) = pi match {
         case _:IsDoc => state.reader.readModule
         case _:IsMod => state.reader.readDeclaration
      }
      val pu = ParsingUnit(state.makeSourceRef(treg), context, text, state.namespaces)
      oi.fromString(objectParser, pi.docHome, pu)(state.errorCont)
   }
}
