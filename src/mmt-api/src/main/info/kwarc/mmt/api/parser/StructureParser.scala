package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import documents._
import libraries._
import archives.{BuildResult, BuildSuccess, BuildTask, LogicalDependency}
import checking.Interpreter
import frontend.Controller
import modules._
import notations._
import objects._
import opaque._
import patterns._
import symbols._
import utils._

import scala.util.Try

/**
 * This class bundles all state that is maintained by a [[StructureParser]]
 *
 * @param reader the input stream, from which the parser reads (see ps below)
 * @param ps the encapsulated input that contains the buffered reader (also encapsulated in reader!)
 */
class ParserState(val reader: Reader, val ps: ParsingStream, val cont: StructureParserContinuations) {
  /** all interpretation instructions applying to the current position, inside out */
  var _iiContext = new InterpretationInstructionContext(ps.nsMap)
  def iiContext = _iiContext

  def namespaces = iiContext.namespaces
  
  /** the position at which the current StructuralElement started */
  var startPosition = reader.getNextSourcePosition

  def copy(rd: Reader = reader): ParserState = {
    val s = new ParserState(rd, ps, cont)
    s._iiContext = _iiContext.copy()
    s
  }

  def makeSourceRef(reg: SourceRegion) = SourceRef(ps.source, reg)

  val errorCont = cont.errorCont
}

case class ParserContext(state: ParserState, docContext: List[InterpretationInstruction])

/** matches the keyword for a view */
object ViewKey {
  def unapply(s: String): Option[String] = s match {
    case "view" | "morphism" => Some(s)
    case _ => None
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

  // ******************************* the entry points

  def apply(ps: ParsingStream)(implicit cont: StructureParserContinuations) = {
    val (se, _) = apply(new ParserState(new Reader(ps.stream), ps, cont))
    se
  }

  private def apply(state: ParserState): (StructuralElement, ParserState) = {
    state.ps.parentInfo match {
       case IsRootDoc(dpath) =>
          val doc = new Document(dpath, FileLevel, initNsMap = state.namespaces)
          seCont(doc)(state)
          logGroup {
            readInDocument(doc)(state)
          }
          end(doc)(state)
          (doc, state)
       case IsRootMod(mpath) => throw LocalError("unsupported")
       // hack: we return the last element of the IsDoc/IsMod, assuming that's the only one that was parsed
       case IsDoc(dp) =>
          val doc = controller.globalLookup.getAs(classOf[Document],dp)
          readInDocument(doc)(state)
          (doc.getDeclarations.lastOption.getOrElse {
            throw ParseError(dp + " is empty: " + state)
          },state)
       case IsMod(mp, rd) =>
          val mod = controller.globalLookup.getAs(classOf[ModuleOrLink],mp)
          readInModule(mod, mod.getInnerContext, new Features(Nil,Nil))(state)
          (mod.getDeclarations.lastOption.getOrElse {
            throw ParseError(mp + " is empty: " + state)
          }, state)
    }
  }

  // *********************************** interface to the controller: add elements and errors etc.

  /**
   * A continuation function called on every StructuralElement that was found
   *
   * For container elements (documents, modules with body), this must be called on the empty element first
   * and then on each child, finally end(se) must be called on the container element.
   * This holds accordingly for nested declared modules. 
   */
  protected def seCont(se: StructuralElement)(implicit state: ParserState) {
    log(se.toString)
    val reg = currentSourceRegion
    SourceRef.update(se, state.makeSourceRef(reg))
    try {
      controller.add(se)
      state.ps.reportProgress(Parsed(se))
      if (!state.ps.isKilled) {
        state.cont.onElement(se)
      }
    } catch {case e: Error =>
      val srcerr = makeError(reg, "error while adding successfully parsed element " + se.path, Some(e))
      errorCont(srcerr)
    }
  }
  /** called at the end of a document or module, does common bureaucracy */
  protected def end(s: ContainerElement[_])(implicit state: ParserState) {
    //extend source reference until end of element
    SourceRef.get(s) foreach {r =>
      SourceRef.update(s, r.copy(region = r.region.copy(end = state.reader.getLastReadSourcePosition)))
    }
    if (!state.ps.isKilled) {
      state.cont.onElementEnd(s)
    }
    controller.endAdd(s)
    log("end " + s.path)
  }

  /** the region from the start of the current structural element to the current position */
  protected def currentSourceRegion(implicit state: ParserState) =
    SourceRegion(state.startPosition, state.reader.getLastReadSourcePosition)

  /** like seCont but may wrap in NestedModule */
  private def moduleCont(m: Module, par: HasParentInfo)(implicit state: ParserState) {
    val se = par match {
       case IsDoc(dp) =>
         m.parentDoc = Some(dp)
         m
       case IsMod(mp, ln) =>
          // mp.name / mname == m.name
          val mname = m.name.dropPrefix(mp.name).getOrElse {throw ImplementationError("illegal name of nested module")}
          val nm = new NestedModule(OMMOD(mp), mname, m)
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
  private def puCont(pu: ParsingUnit)(implicit state: ParserState): ParseResult = {
    def default = DefaultObjectParser(pu)(state.errorCont)
    if (state.ps.isKilled) return default
    try {
      pu.diesWith(state.ps)
      objectParser(pu)(state.errorCont)
    } catch {
      case e: Error =>
        val se = makeError(pu.source.region, "error in object, recovered by using default parser", Some(e))
        state.errorCont(se)
        default
    }
  }

  /**
   * A continuation function called on every error that occurred
   */
  protected def errorCont(e: => SourceError)(implicit state: ParserState) = {
    state.errorCont(e)
  }
  /** convenience function to create SourceError's */
  protected def makeError(reg: SourceRegion, s: String, causedBy: Option[Exception] = None)(implicit state: ParserState) = {
    val msg = s + causedBy.map(": " + _.getMessage).getOrElse("")
    val e = SourceError("structure-parser", state.makeSourceRef(reg), msg)
    causedBy.foreach {c => e.setCausedBy(c)}
    e
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
      LocalName.parse(s, state.namespaces)
    }
    catch {
      case e: ParseError =>
        throw makeError(reg, "invalid identifier: " + e.getMessage)
    }
  }

  /** read a DPath from the stream
    * throws SourceError iff ill-formed or empty
    */
  def readDPath(implicit state: ParserState): DPath = {
    val (s, reg) = state.reader.readToSpace
    if (s == "")
      throw makeError(reg, "MMT URI expected")
    try {
      val p = Path.parseD(s, state.namespaces)
      p
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
 *
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
 *
   * @return the raw string, the region, and the parsed term
   */
  def readParsedObject(context: Context, topRule: Option[ParsingRule] = None)(implicit state: ParserState): (String, SourceRegion, ParseResult) = {
    val (obj, reg) = state.reader.readObject
    val pu = ParsingUnit(state.makeSourceRef(reg.copy(end = reg.end-1)), context, obj, state.iiContext, topRule)
    val pr = puCont(pu)
    (obj, reg.copy(end = reg.end-1), pr)
  }

  private def doComponent(tc: TermContainer, cont: Context)(implicit state: ParserState) {
    val (obj, _, pr) = readParsedObject(cont)
    tc.read = obj
    tc.parsed = pr.toTerm
  }

  /** like doComponent, but expects to find a context (using contextRule notation) */
  private def doContextComponent(cc: ContextContainer, context: Context)(implicit state: ParserState) {
    val (obj, reg, pr) = readParsedObject(context, Some(Context.parsingRule))
    cc.read = obj
    val cont: Context = pr.term match {
      case Context.AsTerm(cont) =>
        cont
      case _: OMSemiFormal =>
        // recover from parsing errors
        Context.empty
      case _ =>
        errorCont(makeError(reg, "not a context: " + controller.presenter.asString(pr.toTerm)))
        Context.empty
    }
    cc.parsed = cont
    cc.unknowns = pr.unknown
    cc.free = pr.free
  }

  private def doParameterComponent(context : Context)(implicit state: ParserState): List[Term] = {
    val (obj,reg,pr) = readParsedObject(context,Some(Context.instanceParsingRule))
    // TODO a) SourceRefs b) Free and unknown variables
    pr.term match {
      case Context.ParamsAsTerm(ls) => ls
      case _ => Nil
    }
  }

  private def readMPathWithParameters(newBase: Path, context : Context)(implicit state: ParserState) : (SourceRef,MPath,List[Term]) = {
    val (fromRef, from) = readMPath(newBase)
    val tms = if (state.reader.startsWith("(")) {
      doParameterComponent(context)
    } else Nil
    (fromRef,from,tms)
  }

  private def doNotation(c: NotationComponentKey, nc: NotationContainer, treg: SourceRegion)(implicit state: ParserState) {
    val notString = state.reader.readObject._1
    if (nc(c).isDefined)
      errorCont(makeError(treg, "notation of this constant already given, ignored"))
    else {
      val notation = try {
        TextNotation.parse(notString, state.namespaces)
      } catch {case p: ParseError =>
        throw makeError(treg, "error in notation", Some(p))
      }
      SourceRef.update(notation,state.makeSourceRef(SourceRegion(treg.start,state.reader.getLastReadSourcePosition)))
      nc(c) = notation
    }
  }

  /** resolve a name in a realized theory (e.g, the domain of a link) and report error on failure */
  protected def resolveDeclarationName[A <: Declaration](cls: Class[A], parent: ModuleOrLink, name: LocalName)(implicit state: ParserState) = {
    val rs = controller.globalLookup.resolveRealizedName(parent, name)
    rs match {
      case Nil =>
        // new local declaration
        name
      case List(p) =>
        p.toLocalName
      case _ =>
        errorCont(makeError(currentSourceRegion, "name is ambiguous, it might refer to any of " + rs.mkString(", ")))
        name
      // this used to also check if the resolved declaration has the right feature
    }
  }

  // *************** the two major methods for reading in documents and modules

  /** the main loop for reading declarations that can occur in documents
    *
    * @param doc the containing Document (must be in the controller already)
    */
  private def readInDocument(doc: Document)(state: ParserState) {
    // state argument is not implicit because we have to copy the state whenever we recurse into a nested declaration
    implicit val stateI = state
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
          val d = new Document(dpath, SectionLevel, initNsMap = state.namespaces)
          seCont(d)
          logGroup {
            readInDocument(d)(state)
          }
          end(d)
          //TODO awkward hack, avoid the FS delimiter of d to make the end-of-document check doc succeed as well
          state.reader.forceLastDelimiter(Reader.GS.toChar.toInt)
        case "ref" =>
          val (_,path) = readMPath(DPath(state.namespaces.default))
          seCont(MRef(doc.path,path))
        case "/" =>
            val oe = readOpaque(parentInfo, Context.empty)
            seCont(oe)
        case k if InterpretationInstruction.all contains k =>
          val (text,reg) = state.reader.readModule
          val ii = InterpretationInstruction.parse(controller, doc.path, k + " " + text, state.namespaces)
          state.iiContext.process(ii)
          seCont(ii)
        case "theory" =>
          readTheory(parentInfo, Context.empty)(state)
        case ViewKey(_) => readView(parentInfo, Context.empty, isImplicit = false)(state)
        case "implicit" =>
          val (keyword2, reg2) = state.reader.readToken
          keyword2 match {
            case ViewKey(_) => readView(parentInfo, Context.empty, isImplicit = true)
            case _ => throw makeError(reg2, "only views can be implicit here")
          }
        case k =>
          getParseExt(doc, k) match {
            // first look for a parser extension
            case Some(extParser) =>
              val (mod, mreg) = state.reader.readModule
              val reader = Reader(mod)
              reader.setNextSourcePosition(mreg.start)
              val pea = new ParserExtensionArguments(this, state.copy(reader), doc, k)
              extParser(pea) foreach {
                case m: Module =>
                  moduleCont(m, parentInfo)
                  //TODO unclear if end(m) should be called; presumably yes if m is declared
                case _ => throw makeError(reg, "parser extension returned non-module")
              }
            case None =>
              // other keywords are treated as module level features
              controller.extman.getOrAddExtension(classOf[ModuleLevelFeature], k) match {
                case Some(_) =>
                  // not actually using the feature (there might be multiple for the same keyword); just checking that at least one exists
                  readDerivedModule(parentInfo, Context.empty, k)
                case None =>
                  throw makeError(reg, "unknown keyword: " + k)
              }
          }
      }
      // check that the reader is at the end of a module level declaration, throws error otherwise
      if (!state.reader.endOfModule) {
        val (rest, reg) = state.reader.readModule
        if (rest != "")
          throw makeError(reg, "extraneous tokens, ignored: " + rest)
      }
    } catch {
      case e: Error =>
        val se = e match {
          case e: SourceError => e
          case e => makeError(currentSourceRegion, "error in module", Some(e))
        }
        errorCont(se)
        if (!state.reader.endOfModule)
          state.reader.readModule
    }
    readInDocument(doc)(state) // compiled code is not actually tail-recursive
  }

  /** the main loop for reading declarations that can occur in a theory
 *
    * @param mod the containing module (added already)
    * @param context the context (including the containing module)
    * @param features the patterns and structural features of the meta-theory (precomputed in readInDocument)
    *
    * this function handles one declaration if possible, then calls itself recursively
    */
  def readInModule(mod: ModuleOrLink, context: Context, features: Features)(state: ParserState) {
     readInModuleAux(mod, mod.asDocument.path, context, features)(state)
  }

  private def readInModuleAux(mod: ModuleOrLink, docHome: DPath, context: Context, features: Features)(state: ParserState) {
    // state argument is not implicit because we have to copy the state whenever we recurse into a nested declaration
    implicit val stateI = state
    //This would make the last RS marker of a module optional, but it's problematic with nested modules.
    //if (state.reader.endOfModule) return
    val mpath = mod.modulePath
    /** the root name of all sections and the LocalName of the currentSection */
    val docRoot = mpath.toDPath
    val currentSection = docHome.dropPrefix(docRoot).getOrElse {
       throw ImplementationError("document home must extend content home")
    }
    lazy val parentInfo = IsMod(mpath, currentSection)
    /* complete declarations should only be added through this method
     * Incomplete container elements must be added separately, e.g., as in readStructure.
     */
    def addDeclaration(d: Declaration) {
       d.setDocumentHome(currentSection)
       seCont(d)
       d match {
         case ce: ContainerElement[_] =>
           // if a container element is added in one go (e.g., includes, instances), we need to also call the end of element hook
           end(ce)
         case _ =>
       }
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
          val c = readConstant(name, mod, context)
          addDeclaration(c)
        //Include
        case "include" | "realize" =>
              // either `include df` or `include tp US = df`
              val tc = new TermContainer // first term, i.e., tp or df
              doComponent(tc, context)
              val (tpC, dfC) = if (state.reader.endOfDeclaration) {
                // only one term provided
                val lup = controller.globalLookup
                mod match {
                  case _:Theory =>
                    // tc must be the type, no definiens
                    (tc, new TermContainer)
                  case _:DerivedDeclaration =>
                    // tc must be the type, no definiens
                    (tc, new TermContainer)
                  case link: Link =>
                    // a single theory is a shortcut for including its identity morphism (.get succeeds because term was read above)
                    tc.get.get match {
                      case t @ OMMOD(p) => 
                        lup.getO(p) match {
                          case Some(_: Theory) =>
                            tc.parsed = OMIDENT(t).from(t)
                          case _ =>
                        }
                      case _ =>
                    }
                    // tc is the definiens, now infer the type the 
                    // type inference belongs to checking, but it's needed already for the name of the include
                    val tp = Morph.domain(tc.get.get)(lup).getOrElse {
                      fail("could not infer domain of included morphism")
                    }
                    (TermContainer(tp), tc)
                }
              } else {
                // first term was type
                readDelimiter("=")
                val dfC = new TermContainer
                doComponent(dfC, context)
                (tc, dfC)
              }
              val name = tpC.get match {
                case Some(OMPMOD(p,_)) => LocalName(p)
                case _ =>
                  fail("domain must be atomic: " + tpC.get)
              }
              // includes into theories are implicit morphisms
              val isImplicit = mod match {
                case _: AbstractTheory => true
                case _ => false
              }
              // realizations must be total
              val isTotal = keyword == "realize"
              val as = new Structure(mod.toTerm, name, tpC, dfC, isImplicit=isImplicit, isTotal=isTotal)
              addDeclaration(as)
        case "structure" => readStructure(parentInfo, mod, context, isImplicit = false, isTotal = false)
        case "theory" => readTheory(parentInfo, context)
        case ViewKey(_) => readView(parentInfo, context, isImplicit = false)
        case k if k.forall(_ == '#') =>
            val currentLevel = currentSection.length
            val thisLevel = k.length
            val (nameTitle,reg) = state.reader.readDeclaration
            // close all sections from currentLevel up to and including thisLevel
            if (thisLevel <= currentLevel) {
               Range(currentLevel,thisLevel-1,-1).foreach {l =>
                  mod.asDocument.getLocally(currentSection.take(l)).foreach {
                    case d: Document => end(d)
                  }
               }
               nextSection = currentSection.take(thisLevel-1)
            }
            if (nameTitle.isEmpty) {
               // just end a section: #...#
               if (thisLevel > currentLevel) {
                  throw makeError(reg, s"no document at level $thisLevel open")
               }
            } else {
               // additionally begin a new section: #...# :NAME TITLE  or #...# TITLE
               if (thisLevel > currentLevel+1) {
                  throw makeError(reg, s"no document at level ${thisLevel-1} open")
               }
               val (name,title) = if (nameTitle.startsWith(":")) {
                  val pos = nameTitle.indexWhere(_.isWhitespace)
                  (nameTitle.substring(1,pos),nameTitle.substring(pos).trim)
               } else {
                  // at this point, nextSection is the current section, i.e., the parent of the one to be opened
                  val name = mod.asDocument.getLocally(nextSection) match {
                     case Some(d) => "section_" + (d.getDeclarations.length+1)
                     case _ => throw ImplementationError("section not found")
                  }
                  (name, nameTitle)
               }
               nextSection = nextSection / name
               val innerDoc = new Document(docRoot / nextSection, SectionInModuleLevel, contentAncestor = Some(mod))
               NarrativeMetadata.title.update(innerDoc, title)
               seCont(innerDoc)
            }
        case "/" =>
            val oe = readOpaque(parentInfo, context)
            seCont(oe)
        //instances of patterns
        case "instance" =>
          mod match {
            case thy: Theory =>
              val patS = readName
              val pat = listmap(features.patterns, patS).getOrElse {
                fail("unknown pattern: " + patS)
              }
              val i = readInstance(pat, thy.path)
              addDeclaration(i)
            case link: Link =>
              fail("instance declaration in link")
          }
        case "implicit" =>
          val (keyword2, reg2) = state.reader.readToken
          keyword2 match {
            case ViewKey(_) => readView(parentInfo, context, isImplicit = true)
            case "structure" => readStructure(parentInfo, mod, context, isImplicit = true, isTotal = false)
            case _ => throw makeError(reg2, "only links can be implicit here")
          }
        case "total" =>
          val (keyword2, reg2) = state.reader.readToken
          keyword2 match {
            case "structure" => readStructure(parentInfo, mod, context, isImplicit = false, isTotal = true)
            case _ => throw makeError(reg2, "only structures can be total")
          }
        case k =>
          // other keywords are treated as ...
          val featureOpt = listmap(features.features, k)
          featureOpt match {
            case Some(sf) =>
              // 0) a derived declarations for a StructuralFeature visible to the theory
              readDerivedDeclaration(sf, parentInfo, context)
            case None =>
              val patOpt = listmap(features.patterns, LocalName.parse(k))
              patOpt match {
                case Some(pattern) =>
                  // 1) an instance of a Pattern with LocalName k visible in meta-theory
                  val i = readInstance(pattern, mpath)
                  addDeclaration(i)
                case None =>
                  val parsOpt = getParseExt(mod, k)
                  if (parsOpt.isDefined) {
                    // 2) a parser extension identified by k
                    val (decl, reg) = state.reader.readDeclaration
                    val reader = Reader(decl)
                    reader.setNextSourcePosition(reg.start)
                    val se = if (currentSection.length == 0) mod
                    else mod.asDocument.getLocally(currentSection).getOrElse {
                      throw ImplementationError("section not found in module")
                    }
                    val pea = new ParserExtensionArguments(this, state.copy(reader), se, k, context)
                    val dO = parsOpt.get.apply(pea)
                    dO foreach {
                      case d: Declaration => addDeclaration(d)
                      case _ => throw makeError(reg, "parser extension returned non-declaration")
                    }
                  } else {
                    // 3) a constant with name k
                    val name = LocalName.parse(k)
                    val c = readConstant(name, mod, context)
                    addDeclaration(c)
                  }
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
          case _ => makeError(currentSourceRegion, "unknown error in declaration", Some(e))
        }
        errorCont(se)
        if (!state.reader.endOfDeclaration)
          state.reader.readDeclaration
    }
    readInModuleAux(mod, docRoot / nextSection, context, features)(state) // compiled code is not actually tail-recursive
  }

  private val definiensDelimiter = List("abbrev", "extends", ":=")
  
  /** auxiliary function to read Theories
    *
    * @param parent the containing document or module (if any)
    * @param context the context (excluding the theory to be read)
    */
  private def readTheory(parent: HasParentInfo, context: Context)(implicit state: ParserState) {
    val rname = readName
    val (ns, name) = parent match {
      case IsDoc(doc) =>
        val ns = DPath(state.namespaces.default)
        val mref = MRef(doc, ns ? rname)
        mref.setOrigin(GeneratedMRef)
        seCont(mref)
        (ns, rname)
      case IsMod(mod,_) =>
        (mod.doc, mod.name / rname)
    }
    val tpath = ns ? name
    var delim = state.reader.readToken
    val metaReg = if (delim._1 == ":") {
      val (r,m) = readMPath(tpath)
      delim = state.reader.readToken
      Some((m,r))
    } else
      None
    val meta = (metaReg,parent) match {
      case (Some((p,_)),_) => Some(p)
      case _ => state.iiContext.meta
    }
    val contextMeta = meta match {
      case Some(p) => context ++ p
      case _ => context
    }
    val paramC = new ContextContainer
    if (delim._1 == ">") {
      doContextComponent(paramC, contextMeta)
      delim = state.reader.readToken
    }
    val contextMetaParams = paramC.get match {
      case None => contextMeta
      case Some(params) => contextMeta ++ params
    }
    val dfC = new TermContainer
    val hasDef = definiensDelimiter contains delim._1
    if (hasDef) {
      doComponent(dfC, contextMetaParams) // DefComponent
    }
    val t = new Theory(ns, name, meta, paramC, dfC)
    metaReg foreach {case (_,r) => SourceRef.update(t.metaC.get.get, r)} //awkward, but needed attach a region to the meta-theory; same problem for structure domains
    moduleCont(t, parent)
    if (!hasDef) {
      if (delim._1 == "=") {
        val features = getFeatures(contextMeta)
        logGroup {
          readInModule(t, context ++ t.getInnerContext, features)(state.copy())
        }
      } else {
        throw makeError(delim._2, "':' or '=' or 'abbrev' expected")
      }
    }
    end(t)
  }

  /** auxiliary function to read views
    *
    * @param parent the containing document/module
    * @param context the context (excluding the view to be read)
    * @param isImplicit whether the view is implicit
    */
  private def readView(parent: HasParentInfo, context: Context, isImplicit: Boolean)(implicit state: ParserState) {
    val rname = readName
    val (ns, name) = parent match {
      case IsDoc(doc) =>
        val ns = DPath(state.namespaces.default)
        val mref = MRef(doc, ns ? rname)
        mref.setOrigin(GeneratedMRef)
        seCont(mref)
        (ns, rname)
      case IsMod(mod,_) =>
        (mod.doc, mod.name / rname)
    }
    val vpath = ns ? name
    readDelimiter(":")
    val (fromRef, fromPath, fromArgs) = readMPathWithParameters(vpath,context)
    val from = OMPMOD(fromPath,fromArgs)
    SourceRef.update(from, fromRef)
    readDelimiter("->", "→")
    var tos: List[(SourceRef,MPath,List[Term])] = Nil
    var delim: (String,SourceRegion) = ("",null)
    do {
      val to = readMPathWithParameters(vpath,context)
      tos = tos ::: List(to)
      delim = state.reader.readToken
    } while (delim._1 == "+")
    val to = tos match {
      case (ref,p,args) :: Nil =>
        val t = OMPMOD(p,args)
        SourceRef.update(t,ref)
        t
      case _ =>
        val ds= tos map {case (r,p,as) =>
          val d = IncludeVarDecl(p,as)
          SourceRef.update(d,r)
          d
        }
        ComplexTheory(Context(ds:_*))
    }
    delim._1 match {
      case "abbrev" =>
        val (_, _, df) = readParsedObject(context)
        val v = View(ns, name, from, to, TermContainer.asParsed(df.toTerm), isImplicit)
        moduleCont(v, parent)
        end(v)
      case "=" =>
        val v = View(ns, name, from, to, TermContainer.empty(), isImplicit)
        moduleCont(v, parent)
        logGroup {
          readInModule(v, context ++ v.getInnerContext, noFeatures)(state.copy())
        }
        end(v)
      case _ =>
        throw makeError(delim._2, List("abbrev","=").map("'" + _ + "'").mkString(" or ") + "expected")
    }
  }
  
  private def readDerivedModule(parent: HasParentInfo, context: Context, feature: String)(implicit state: ParserState) {
    val rname = readName
    val (ns, name) = parent match {
      case IsDoc(doc) =>
        val ns = DPath(state.namespaces.default)
        val mref = MRef(doc, ns ? rname)
        mref.setOrigin(GeneratedMRef)
        seCont(mref)
        (ns, rname)
      case IsMod(mod,_) =>
        (mod.doc, mod.name / rname)
    }
    val mpath = ns ? name
    var delim = state.reader.readToken
    val metaReg = if (delim._1 == ":") {
      val (r,m) = readMPath(mpath)
      delim = state.reader.readToken
      Some((m,r))
    } else
      None
    val meta = (metaReg,parent) match {
      case (Some((p,_)),_) => Some(p)
      case _ => state.iiContext.meta
    }
    val contextMeta = meta match {
      case Some(p) => context ++ p
      case _ => context
    }
    val dfC = new TermContainer
    // there is either a definiens or a body
    val hasDef = definiensDelimiter contains delim._1 
    if (hasDef) {
      doComponent(dfC, contextMeta) // DefComponent
    }
    val dm = new DerivedModule(feature, ns, name, meta, new TermContainer(), dfC, new NotationContainer())
    //metaReg foreach {case (_,r) => SourceRef.update(dm.metaC.get.get, r)} //awkward, but needed attach a region to the meta-theory; same problem for structure domains
    moduleCont(dm, parent)
    if (!hasDef) {
      if (delim._1 == "=") {
        val features = getFeatures(contextMeta)
        logGroup {
          readInModule(dm, context ++ dm.getInnerContext, features)(state.copy())
        }
      } else {
        throw makeError(delim._2, "':' or '=' or ':=', or 'abbrev' expected")
      }
    }
    end(dm)
  }

  /**
   * allow to control certain parser extensions
   * i.e. those with side effects like [[RuleConstantParser]]
   */
  protected def getParseExt(se: StructuralElement, key: String): Option[ParserExtension] =
    controller.extman.getParserExtension(se, key)

  /** holds the structural features and patterns that are available during parsing */
  protected class Features(val features: List[(String,StructuralFeature)], val patterns: List[(LocalName,(StructuralFeature,DerivedDeclaration))]) {
    def +(f : Features) = new Features((features ::: f.features).distinct,(patterns ::: f.patterns).distinct)
  }
  val noFeatures = new Features(Nil,Nil)

  /** auxiliary function to collect all structural feature rules in a given context */
  protected def getFeatures(mp: MPath): Features = {
    controller.simplifier(mp)
    var fs: List[(String,StructuralFeature)] = Nil
    var ps: List[(LocalName,(StructuralFeature,DerivedDeclaration))] = Nil
    controller.globalLookup.forDeclarationsInScope(OMMOD(mp)) {case (p,m,d) => d match {
      //TODO translate via m where necessary
      case rc: RuleConstant => rc.df.foreach {
        case r: StructuralFeatureRule =>
          controller.extman.get(classOf[StructuralFeature], r.feature) match {
            case Some(sf) =>
              fs ::= r.feature -> sf
            case None =>
              // maybe generate warning; error will be thrown anyway when the rule constant is checked
          }
        case _ =>
      }
      case dd @ Pattern(_,_,_,_) =>
        controller.extman.get(classOf[StructuralFeature], Instance.feature) match {
          case Some(sf) =>
            ps ::= dd.name -> (sf, dd)
          case None =>
        }
      case _ =>
    }}
    new Features(fs, ps)
  }
  protected def getFeatures(cont : Context) : Features = cont.collect({
    case IncludeVarDecl(_,OMPMOD(mp,_),_) => getFeatures(mp)
  }).foldLeft(noFeatures)((a,b) => a+b)

  /** auxiliary method for reading declarations that reads a list of components
   *  @param context the current context
   *  @param expected the components to read as (initial delimiter, (key, container))
   *  @param until an initial delimiter that stops parsing components
   *  @return true if the delimiter until was found; false if end of declaration was found
   */
  // TODO use this in readConstant
  private def readComponents(context: Context, expected: List[(String,(ComponentKey,ComponentContainer))], until: Option[String])(implicit state: ParserState): Boolean = {
     while (!state.reader.endOfDeclaration) {
       val (delim, treg) = state.reader.readToken
       if (until contains delim) return true
       listmap(expected, delim) match {
          case Some((key,cont)) =>
            if (cont.isDefined) {
              errorCont(makeError(treg, s"component $key already given, ignored"))
              state.reader.readObject
            } else {
              (key,cont) match {
                case (_, tc: TermContainer) =>
                  doComponent(tc, context) // key
                case (_, cc: ContextContainer) =>
                  doContextComponent(cc, context)
                case (nk: NotationComponentKey, nc: NotationContainer) =>
                  doNotation(nk, nc, treg)
                case _ => throw ImplementationError("illegal component")
              }
            }
          case None =>
            errorCont(makeError(treg, s"component $delim not expected, ignored"))
            state.reader.endOfObject
       }
     }
     return false
  }
  /** auxiliary function to build input for readComponents for notation components */
  private def notationComponentSpec(nc: NotationContainer) =
    List("#" -> ParsingNotationComponent, "##" -> PresentationNotationComponent).map{case (s,k) => s -> (k,nc)}

  /** reads the components of a [[Constant]]
    *
    * @param givenName the name of the constant
    * @param mod the containing element
    * @param context the context for term components
    */
  private def readConstant(givenName: LocalName, mod: ModuleOrLink,
                           context: Context)(implicit state: ParserState): Constant = {
    val parent = mod.modulePath
    val name = resolveDeclarationName(classOf[Constant], mod, givenName)
    val cpath = parent ? name
    //initialize all components as omitted
    val tpC = new TermContainer
    val dfC = new TermContainer
    var al: List[LocalName] = Nil
    val nt = new NotationContainer
    var rl: Option[String] = None
    val cons = Constant(OMMOD(parent), name, Nil, tpC, dfC, None, nt)
    // every iteration reads one delimiter and one object
    // @ alias or : TYPE or = DEFINIENS or #(#) NOTATION
    val keys = List(":", "=", "#", "##", "@", "role")
    val keyString = keys.map("'" + _ + "'").mkString(", ")

    while (!state.reader.endOfDeclaration) {
      val (delim, treg) = state.reader.readToken
      try {
        // branch based on the delimiter
        delim match {
          case ":" =>
            if (tpC.read.isDefined) {
              errorCont(makeError(treg, "type of this constant already given, ignored"))
              state.reader.readObject
            } else
              doComponent(tpC, context) // TypeComponent
          case "=" =>
            if (dfC.read.isDefined) {
              errorCont(makeError(treg, "definiens of this constant already given, ignored"))
              state.reader.readObject
            } else
              doComponent(dfC, context) // DefComponent
          case "#" =>
            doNotation(ParsingNotationComponent, nt, treg)
          case "##" =>
            doNotation(PresentationNotationComponent, nt, treg)
          case "@" =>
            val (str, _) = state.reader.readObject
            al ::= LocalName.parse(str)
          case "role" =>
            val (str, _) = state.reader.readObject
            rl = Some(str)
          case k => getParseExt(cons, k) match {
            case Some(parser) =>
              val (obj, reg) = state.reader.readObject
              val reader = Reader(obj)
              reader.setNextSourcePosition(reg.start)
              val pea = new ParserExtensionArguments(this, state.copy(reader), cons, k, context)
              val tO = parser(pea)
              tO foreach {
                case d => throw makeError(reg, "parser extension in constant may not return anything")
              }
            case None =>
              if (!state.reader.endOfDeclaration) {
                errorCont(makeError(treg, s"expected $keyString, found $k (note that '$givenName' was treated as a constant name because it was not recognized as a keyword)"))
              } else if (k != "") {
                if (!state.reader.endOfObject)
                  state.reader.readObject
                errorCont(makeError(treg, "expected " + keyString + ", ignoring the next object"))
              }
          }
        }
      } catch {
        case e: Exception =>
          errorCont(makeError(treg, " error in object", Some(e)))
          if (!state.reader.endOfObject)
             state.reader.readObject
      }
    }
    val constant = Constant(OMMOD(parent), name, al, tpC, dfC, rl, nt)
    constant.metadata = cons.metadata
    constant
  }

  /** read structures and calls continuations
    *
    * @param parentInfo the containing module
    * @param context the context (excluding the structure to be read)
    * @param isImplicit whether the structure is implicit
    */
  private def readStructure(parentInfo: IsMod, mod: ModuleOrLink, context: Context,
                            isImplicit: Boolean, isTotal: Boolean)(implicit state: ParserState) {
    val link = mod match {case l: Link => Some(l) case _ => None}
    val givenName = readName
    val home = OMMOD(parentInfo.modParent)
    val name = resolveDeclarationName(classOf[Structure], mod, givenName)
    val spath = parentInfo.modParent ? name
    // the type and the code to set it (if provided)
    val tpC = new TermContainer
    // shared code for creating the structure
    def createStructure(df: Option[Term]) = {
      val dfC = TermContainer(df)
      val s = new Structure(home, name, tpC, dfC, isImplicit, isTotal)
      s.setDocumentHome(parentInfo.relDocParent)
      seCont(s)
      s
    }
    // read the optional type
    val (t, r) = state.reader.readToken
    var token = t
    var reg = r
    // read the type (if any)
    if (token == ":") {
      //doComponent(TypeComponent, tpC, context)
      val tp = OMMOD(readMPath(spath)._2)
      tpC.parsed = tp
      // read next token (if any)
      if (state.reader.endOfDeclaration) {
        // empty declared Structure: s: tp RS
        if (link.isDefined) {
          // declared structure only allowed in a theory
          throw makeError(reg, "'=' expected, within structure " + spath)
        }
        val s = createStructure(None)
        end(s)
        return
      }
      // read the next token
      val (t,r) = state.reader.readToken
      token = t
      reg = r
    } else {
      if (link.isEmpty) {
        // type may only be omitted in a link
        throw makeError(reg, "':' expected, within structure " + spath)
      }
    }
    // read the body or the definiens
    val defDelim = if (link.isDefined) "=" else "abbrev"
    if (token == defDelim) {
      // read the definiens of a DefinedStructure
      val (targetStr, reg) = state.reader.readObject
      val nsMap = state.namespaces(parentInfo.modParent)
      val sref = state.makeSourceRef(reg)
      // TODO target should be parsed as a morphism expression, in particular compositions are allowed; for now we just parse a reference to a structure or a view
      val target = try {
        val p = Path.parseS(targetStr, nsMap)
        OMS(p)
      } catch {case _: Error =>
        try {
          val p = Path.parseM(targetStr, nsMap)
          OMMOD(p)
        } catch {case _: Error =>
          makeError(reg, "only references to structures or views supported", None)
          val pu = ParsingUnit(sref, context, targetStr, state.iiContext, None)
          DefaultObjectParser(pu)(state.errorCont).toTerm
        }
      }
      SourceRef.update(target, sref)
      val s = createStructure(Some(target))
      end(s)
    } else {
      // read the body a DeclaredStructure
      if (link.isDefined) {
        // declared structure only allowed in a theory
        throw makeError(reg, "'=' expected, within structure " + spath)
      }
      if (token != "=") {
        // unexpected delimiter for the body of a declared structure
        throw makeError(reg, "'=' expected, within structure " + spath)
      }
      val s = createStructure(None)
      logGroup {
        readInModule(s, context, noFeatures)(state.copy())
      }
      end(s)
    }
  }

  /** reads a [[DerivedDeclaration]] and calls continuations */
  private def readDerivedDeclaration(feature: StructuralFeature, parentInfo: IsMod, context: Context)(implicit state: ParserState) {
    val parent = parentInfo.modParent
    val pr = feature.getHeaderRule
    val (_, reg, header) = readParsedObject(context, Some(pr))
    val (name, tp) = feature.processHeader(header.term)
    SourceRef.update(tp, state.makeSourceRef(reg))
    val tpC = TermContainer(header.copy(term = tp).toTerm)
    val notC = new NotationContainer
    val compSpecs = notationComponentSpec(notC)
    val equalFound = readComponents(context, compSpecs, Some(feature.bodyDelim))
    // TODO read definiens instead of body
    val dd = new DerivedDeclaration(OMID(parent), name, feature.feature, TermContainer(tp), notC)
    dd.setDocumentHome(parentInfo.relDocParent)
    seCont(dd)
    if (equalFound) {
       //TODO calling the simplifier here is a hack that is not allowed
       //val innerContext = controller.simplifier.elaborateContext(context,feature.getInnerContext(dd))
       val innerContext = feature.getInnerContext(dd)
       val features = getFeatures(parent)
       readInModule(dd.module, context ++ innerContext, features)(state.copy())
    }
    end(dd)
  }

  /** returns an instance of [[InstanceFeature]]
   *
   *  parses 'pattern(name, args) NOTATIONS' where name is a free variable for the name of the instance */
  private def readInstance(instFeatPattern: (StructuralFeature,DerivedDeclaration), tpath: MPath)(implicit state: ParserState): DerivedDeclaration = {
    val (instFeat, pattern) = instFeatPattern
    val context = Context(tpath)
    val patNot = pattern.notC.parsing map {n => ParsingRule(pattern.modulePath, Nil, n)}
    val (_, reg, pr) = readParsedObject(context, patNot)
    val (name,tp) = instFeat.processHeader(pr.term)
    SourceRef.update(tp, state.makeSourceRef(reg))
    val tpC = TermContainer(pr.copy(term = tp).toTerm)
    val nc = NotationContainer()
    readComponents(context, notationComponentSpec(nc), None)
    Instance(OMMOD(tpath), name, tpC, nc)
  }

  private def readOpaque(pi: HasParentInfo, context: Context)(implicit state: ParserState): OpaqueElement = {
      val (format, freg) = state.reader.readToken
      val oi = controller.extman.get(classOf[OpaqueTextParser], format).getOrElse {
        throw makeError(freg, "unknown opaque format: " + format)
      }
      val (text, treg) = pi match {
        case _:IsDoc => state.reader.readModule
        case _:IsMod => state.reader.readDeclaration
      }
      val pu = ParsingUnit(state.makeSourceRef(treg), context, text, state.iiContext)
      oi.fromString(objectParser, pi.docParent, pu)(state.errorCont)
   }
}


/**
  * estimates the [[archives.BuildResult]] of an mmt [[Interpreter]] by using the [[StructureParser]] superficially
  */
trait MMTStructureEstimator {self: Interpreter =>
  private var used: List[MPath] = Nil
  private var provided: List[MPath] = Nil

  override def estimateResult(bt: BuildTask) = {
    if (bt.isDir) BuildSuccess(Nil, Nil)
    else {
      val (dp, ps) = buildTaskToParsingStream(bt)
      used = Nil
      provided = Nil
      parser(ps)(new StructureParserContinuations(bt.errorCont))
      // convert i.e. p?NatRules/NatOnly to p?NatRules
      used = used.map { mp =>
        val steps = mp.name.steps
        if (steps.isEmpty) mp else MPath(mp.parent, List(steps.head))
      }
      used = used.distinct
      provided = provided.distinct
      used = used diff provided
      BuildSuccess(used map LogicalDependency, provided map LogicalDependency)
    }
  }

  private lazy val parser = new KeywordBasedParser(DefaultObjectParser) {
    self.initOther(this)
    private object AddUsed extends StatelessTraverser {
      def traverse(t: Term)(implicit con : Context, init: Unit) = t match {
        case OMMOD(p) =>
          used ::= p
          t
        case _ => Traverser(this, t)
      }
      def apply(tC: AbstractTermContainer, context: Context) {
        tC.get.foreach {t => apply(t, context)}
      }
    }

    override def seCont(se: StructuralElement)(implicit state: ParserState) = se match {
      case t: Theory =>
        provided ::= t.path
        t.meta foreach { m => used ::= m }
        AddUsed(t.dfC, t.parameters)
      case v: View =>
        provided ::= v.path
        AddUsed(v.fromC, Context.empty)
        AddUsed(v.toC, Context.empty)
        AddUsed(v.dfC, Context.empty)
      case s: Structure =>
        AddUsed(s.fromC, Context.empty)
        AddUsed(s.dfC, Context.empty)
      case _ =>
    }

    // below: trivialize methods that are not needed for structure estimation

    override def resolveDeclarationName[A](cls: Class[A], parent: ModuleOrLink, name: LocalName)(implicit state: ParserState) = name
    override def getFeatures(m: MPath) = noFeatures
    override def errorCont(e: => SourceError)(implicit state: ParserState) {}
    override def getParseExt(se: StructuralElement, key: String): Option[ParserExtension] = key match {
      case "rule" => None
      case _ =>
        super.getParseExt(se, key)
    }
    override def end(s: ContainerElement[_])(implicit state: ParserState) {}
  }
}
