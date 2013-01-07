package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import symbols._
import objects._

import scala.collection.mutable.{ListMap,HashMap}

case class ParserState(val reader: Reader) {
   val namespace = new ListMap[String,DPath]
   var defaultNamespace: DPath = utils.mmt.mmtbase
}

trait InDocParser {
   def apply(sp: StructureParser, r: Reader)
}
trait InTheoryParser {
   def apply(sp: StructureParser, r: Reader)
}


// sketch of future reimplementation of TextReader
abstract class StructureParser(controller: Controller) extends frontend.Logger {
   val report = controller.report
   val logPrefix = "structure-parser"
   
   val inDocParsers = new HashMap[String,InDocParser]
   val inTheoryParsers = new HashMap[String,InTheoryParser]
   
   def seCont(se: StructuralElement): Unit
   def puCont(pu: ParsingUnit): Term
   def errorCont(err: SourceError): Unit
   
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

   private def makeError(reg: SourceRegion, s: String) =
      SourceError("structure-parser", SourceRef(null, reg), s)
   
   def readName(implicit state: ParserState) : LocalName = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "name expected")
      try {LocalName.parse(s)}
      catch {case e: ParseError =>
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   def readLocalPath(implicit state: ParserState) : LocalPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "module name expected")
      try {Path.parseLocal(s).toLocalPath}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   def readDPath(base: Path)(implicit state: ParserState) : DPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseD(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   def readMPath(base: Path)(implicit state: ParserState) : MPath = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseM(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
   def readSPath(base: MPath)(implicit state: ParserState) : GlobalName = {
      val (s, reg) = state.reader.readToken
      if (s == "")
         throw makeError(reg, "MMT URI expected")
      try {Path.parseS(s, base)}
      catch {case e: ParseError => 
         throw makeError(reg, "invalid identifier: " + e.getMessage)
      }
   }
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
   
   def readInTheory(thy: DeclaredTheory, patterns: List[(String,GlobalName)])(implicit state: ParserState) {
      if (state.reader.endOfModule) return
      try {
         val (keyword, reg) = state.reader.readToken
         keyword match {
            case "" =>
               if (state.reader.endOfModule) {
                  return
               } else
                  throw makeError(reg, "keyword expected, within theory " + thy).copy(fatal = true)
            case "constant" =>
               val name = readName
               readConstant(name, thy.path)
            case "include" =>
               val from = readMPath(thy.path)
               if (! state.reader.endOfDeclaration) {
                  val (rest, reg) = state.reader.readDeclaration
                  if (rest != "")
                     throw makeError(reg, "end of declaration expected, found and ignored " + rest)
               }
               val incl = PlainInclude(from, thy.path)
               seCont(incl)
            case "pattern" =>
               val name = readName
               //TODO
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
   
   private def readConstant(name: LocalName, tpath: MPath)(implicit state: ParserState) {
      val cpath = tpath ? name
      var tp : Option[Term] = None
      var df : Option[Term] = None
      var nt : Option[TextNotation] = None
      while (! state.reader.endOfDeclaration) {
         val (delim, treg) = state.reader.readToken
         if (! List(":","=","#").contains(delim)) {
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
                  val notString = obj
                  val notation = TextNotation.parse(notString, cpath)
                  nt = Some(notation)
               //TODO read metadata
            }
         }
      }
      val c = new Constant(OMMOD(tpath), name, tp, df, None, nt)
      seCont(c)
   }
   private def readInstance(name: LocalName, pattern: GlobalName)(implicit state: ParserState) {
      
   }
   
   def readInView(view: MPath)(implicit state: ParserState) {
      
   }
   
   //TODO, text syntax for styles?
   //def readInStyle(style: MPath)(implicit state: ParserState) {}
}

class StructureAndObjectParser(controller: Controller) extends StructureParser(controller) {
   def puCont(pu: ParsingUnit): Term = {
      val obj = try {
         controller.termParser(pu)  
      } catch {
         case e: SourceError =>
            errorCont(e)
            DefaultParser(pu)
      }
      obj
   }
   def seCont(se: StructuralElement) {
      log(se.toString)
      controller.add(se)
   }
   private var errors: List[SourceError] = Nil
   def errorCont(err: SourceError) {
      errors ::= err
      report(err)
   }
   def getErrors = errors.reverse
}

