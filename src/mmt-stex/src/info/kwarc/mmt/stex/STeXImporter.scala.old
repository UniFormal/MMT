package info.kwarc.mmt.stex

import java.io.FileNotFoundException

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend._
import info.kwarc.mmt.api.Level.Level
import informal._
import modules._
import notations._
import objects._
import parser._
import symbols._
import utils._

import scala.annotation.tailrec
import scala.util.Try
import scala.xml.{Elem, NamespaceBinding, Node}


/*
class STeXImporter extends Importer {
  val key: String = "stex-omdoc"
  override val logPrefix = "steximporter"
  override val inDim = RedirectableDimension("latexml")

  var docCont: Map[DPath, Document => Unit] = Nil.toMap

  def inExts = List("omdoc") //stex/latexml generated omdoc

  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
  }

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    if (!bt.isDir) {
      try {
        val dpath = bt.narrationDPath
        val src = scala.io.Source.fromFile(bt.inFile.toString)
        val cp = scala.xml.parsing.ConstructingParser.fromSource(src, preserveWS = true)
        val node: Node = cp.document().head
        src.close()
        var used: List[Dependency] = node.child.flatMap(c => estimateModuleDeps(c, bt.narrationDPath)).toList
        //controller.backend.getArchive(id)
        //FileBuildDependency("stex-importer", archive, inPath)
        /*
        println("-------------------")
        println(dpath)
        println(used)
        println(provided)
        println("################")
        */
        BuildSuccess(used.distinct, Nil)
      } catch {
        case e: Exception =>
          log("Error: Cannot estimate dependencies for " + bt.inFile + "due to unexpected error: " + e.getMessage)
          BuildSuccess(Nil, Nil)
      }
    } else {
      BuildSuccess(Nil, Nil)
    }
  }

  def estimateModuleDeps(n: Node, dpath: DPath): List[FileBuildDependency] = n.label match {
    case "theory" =>
      try {
        val tname = DocumentImporter.getNameO(n).get
        val mpath = dpath ? tname
        val deps = n.child flatMap { i =>
          i.label match {
            case "imports" | "uses" =>
              val fromS = (i \ "@from").text.split("#")(0)
              val (group, repo, frags) = DocumentImporter.parseStexPath(fromS, dpath)
              val archive = controller.backend.getArchive(s"$group/$repo").get
              val from = FileBuildDependency("stex-omdoc", archive, frags)
              Some(from)
            case _ => None
          }
        }
        deps.toList
      } catch {
        case e: Exception =>
          Nil
      }
    case _ => Nil //nothing to do
  }

  // var counter = 1

  def importDocument(bt: BuildTask, cont: Document => Unit): BuildResult = {
    val importer = new DocumentImporter(bt,this)
    importer(cont)
  }
/*
  def compileOne(inText: String, dpath: DPath): (String, List[Error]) = {
    val node = scala.xml.XML.loadString(inText) //clearXmlNS(
    val cleanNode = node //scala.xml.Utility.trim(node)
    val errHandler = new ErrorContainer(None)
    translateDocument(cleanNode)(dpath, errHandler)
    val errors = errHandler.getErrors
    errors match {
      case Nil => //returning result
        val docXML = controller.getDocument(dpath).toNodeResolved(controller.globalLookup, true)
        (docXML.toString(), Nil)
      case _ => //reporting errors
        val doc = try {
          controller.getDocument(dpath).toNodeResolved(controller.globalLookup, true).toString()
        } catch {
          case e: Exception => ""
        }
        (doc, errors)
    }
  }
*/
  def logthis(s : String) : Unit = log(s)
  lazy val thiscontroller : Controller = controller
}

case class Missing(d: Dependency) extends Throwable

object DocumentImporter {
  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val omdocNS = "http://omdoc.org/ns"
  val mhBase : DPath = DPath(URI("http://mathhub.info/"))

  def getNameO(n : Node) : Option[LocalName] = {
    val nameS = (n \ s"@{$xmlNS}id").text
    if (nameS.isEmpty) None else Some(LocalName(nameS))
  }

  def getName(n: Node, container: StructuralElement): LocalName = getNameO(n) match {
    case Some(ln) => ln
    case None =>
      LocalName("Anon" + container.getDeclarations.length.toString)
  }


  def parseStexPath(s : String, base: DPath): (String, String, List[String]) = {
    val baseGroup = base.uri.path.head
    val baseArchive = base.uri.path(1)
    //makes path absolute
    @tailrec
    def removeRel(frags: List[String], acc: List[String]): List[String] = frags match {
      case Nil => acc.reverse
      case ".." :: tl if acc.isEmpty => removeRel(tl, acc)
      case ".." :: tl => removeRel(tl, acc.tail)
      case hd :: tl => removeRel(tl, hd :: acc)
    }
    val plainFrags = removeRel(s.split("/").toList, Nil)
    val srcidx = plainFrags.indexOf("source")
    val fragPath = plainFrags.drop(srcidx + 1)

    val (group, archive) = srcidx match {
      case -1 => throw new STeXParseError("Invalid DPath, cannot produce group/project/'source'/path canonical form", Some("did not find 'source' folder in: " + s), None, None)
      case 0 => (baseGroup, baseArchive)
      case 1 => (baseGroup, plainFrags.head)
      case _ => (plainFrags(srcidx - 2), plainFrags(srcidx - 1))
    }
    val frags =  fragPath match {
      case Nil =>
        throw new STeXParseError("Invalid DPath, empty document name", Some(s"Got `$s` as document path"), None, None)
      case _ => fragPath.init // ::: List(fragPath.last /* + ".omdoc" */)
    }
    (group, archive, frags)
  }
}

class DocumentImporter(bt:BuildTask,importer:STeXImporter) {
  import DocumentImporter._

  private val controller = importer.thiscontroller
  private def log(s:String): Unit = importer.logthis(s)

  def add(s: StructuralElement) {
    log("adding " + s.path.toPath)
    controller.add(s)
  }

  def apply(cont: Document => Unit) : BuildResult = try {
    // println(s"$counter. stex-importing " + bt.inPath)
    // counter += 1
    importer.docCont += (bt.narrationDPath -> cont) // to reindex document if needed
    val src = scala.io.Source.fromFile(bt.inFile.toString)
    val cp = scala.xml.parsing.ConstructingParser.fromSource(src, preserveWS = true)
    val node: Node = cp.document().head
    src.close()
    translateDocument(node)(bt.narrationDPath, bt.errorCont)
    val doc = controller.getDocument(bt.narrationDPath)
    cont(doc)
    BuildResult.empty
  } catch {
    case Missing(d) =>
      MissingDependency(List(d), Nil, Nil)
    case e: FileNotFoundException =>
      val file = File(e.getMessage.replace(" (No such file or directory)", ""))
      MissingDependency(List(PhysicalDependency(file)), Nil, Nil)
    case exc: Exception =>
      exc.printStackTrace()
      val err = STeXParseError.from(exc, "Skipping article due to unexpected error", None, None, Some(Level.Fatal))
      bt.errorCont(err)
      BuildResult.empty
  }

  private def getAnonThy(dpath: DPath): Theory = {
    val anonpath = dpath ? OMV.anonymous
    try {
      controller.get(anonpath) match {
        case d: Theory => d
      }
    } catch {
      case e: GetError =>
        val anonthy = Theory.empty(anonpath.doc, anonpath.name, Theory.noMeta) //no meta for now
        val ref = MRef(dpath, anonthy.path)
        controller.add(anonthy)
        controller.add(ref)
        anonthy
    }
  }

  /**
    * Translate a toplevel <omdoc> node
    */
  private def translateDocument(n: Node)(implicit dpath: DPath, errorCont: ErrorHandler) {
    n.label match {
      case "omdoc" =>
        //creating document and implicit theory
        implicit val doc : Document = new Document(dpath, FileLevel)
        add(doc)
        //recursing into children
        n.child.foreach(translateModule)
    }
  }

  /**
    * translate second-level, in-document elements (typically modules)
    */
  private def translateModule(n: Node)(implicit doc: Document, errorCont: ErrorHandler) {
    val sref = parseSourceRef(n, doc.path)
    val dpath = doc.path.^
    try {
      n.label match {
        case "theory" => //create theory
          val name = getName(n, doc)
          val thy = Theory.empty(doc.path.^, name, Theory.noMeta)
          val ref = MRef(doc.path, thy.path)
          add(ref)
          add(thy)
          n.child.foreach(translateDeclaration(_)(doc, thy, errorCont))
          print("")
        case "view" => // TODO fromrepos/torepos once present
          val name = getName(n, doc)
          val fromN = (n\"@from").text
          val toN = (n\"@to").text
          val from = Path.parseM("?"+fromN,NamespaceMap(dpath)) // parseMPath(fromN,dpath)
          val to = Path.parseM("?"+toN,NamespaceMap(dpath)) // parseMPath(toN,dpath)
          val view = View(dpath,name,OMID(from),OMID(to), isImplicit = false)
          val ref = MRef(dpath,view.path)
          add(ref)
          add(view)
          n.child.foreach(translateDeclaration(_)(doc, view, errorCont))
        case "omgroup" => //recurse to find internal modules
          val name = getName(n, doc)
          val newDoc = new Document(doc.path / name, SectionInModuleLevel)
          add(newDoc)
          n.child.foreach(n => translateModule(n)(newDoc, errorCont))
        case "metadata" => //TODO
        case "bibliography" => //TODO
        case "index" => //TODO
        case "oref" =>
          val href = (n \ "@href").text
          //val target = doc.path.^! / (href + ".omdoc")
          //val dref = new DRef(doc.path, target)
          val target = parseRelDPath(href, doc.path)
          val dref = new DRef(doc.path, LocalName.empty ,target)
          add(dref)
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
        case "tableofcontents" => //ignore for now
        case _ =>
          log("CREATED ANON THY because of label: " + n.label)
          translateDeclaration(n: Node)(doc, getAnonThy(doc.path), errorCont)
      }
    } catch {
      case e: STeXError => errorCont(e)
      case e: Exception =>
        val err = STeXParseError.from(e, "Skipping module-level element `" + n.label + "` due to unexpected error", None, sref, None)
    }
  }

  /**
    * translate third level, in-module elements (typically declarations)
    */
  private def translateDeclaration(n: Node, localSection : LocalName = LocalName.empty)(implicit doc: Document, thy: ModuleOrLink, errorCont: ErrorHandler) {
    implicit val dpath : DPath = doc.path.^
    implicit val mpath : MPath = thy.path.toMPath
    val sref = parseSourceRef(n, doc.path)
    try {
      n.label match {
        case "imports" | "uses" => //omdoc import -> mmt (plain) include
          val fromS = (n \ "@from").text
          val from = parseMPath(fromS, dpath)
          if (from != mpath) {
            val include = PlainInclude(from, mpath)
            (n \ "@conservative").text match {
            case "true" =>
              include.metadata.add(sTeXMetaData.conservativeExtension)
            case _ => //nothing to do
            }
            add(include)
          }
        case "symbol" => //omdoc symbol -> mmt constant
          val name = (n \ "@name").text match {
            case "" => getName(n, thy)
            case s => LocalName(s)
          }

          val tpWrapperO = n.child.find(_.label == "type")
          val tpO = tpWrapperO.map(tpN => Obj.parseTerm(rewriteCMP(tpN.child.head), NamespaceMap(dpath)))
          val dfO = None //TODO, get also def
          val const = Constant(OMMOD(mpath), name, Nil, TermContainer(tpO), TermContainer(dfO), None, NotationContainer())
          //adding metadata
          (n \ "@role").text match {
            case "primary" =>
              const.metadata.add(sTeXMetaData.primarySymbol)
            case _ => //nothing to do
          }
          const.setDocumentHome(localSection)
          if (!thy.declares(const.name)) add(const)
        case "definition" => //omdoc definition -> immt flexiformal declaration
          val name = getName(n, thy)
          val targetsS = (n \ "@for").text.split(" ").flatMap(_.split(',')).filter(_ != "")
          val targets = targetsS.toList.distinct map { s =>
            s.split("\\?").toList match {
              case tname :: sname :: Nil =>
                resolveSPath(Some(tname), sname, mpath)(thy,errorCont)
              case sname :: Nil =>
                resolveSPath(None,sname,mpath)(thy,errorCont)
              case l =>
                val error = new STeXParseError(s"Definition has invalid target attribute",
                  Some(s"expected `thyName?symName` found `$s`. Defaulting to current theory and using `${l.last}` as symName"), sref, Some(Level.Warning))
                errorCont(error)
                resolveSPath(None, l.last, thy.path.toMPath)(thy,errorCont)
            }
          }
          val spath = targets.headOption.getOrElse(mpath ? name)
          parseNarrativeObject(n)(dpath, thy, errorCont) match {
            case None => //nothing to do
            case Some(no) =>
              val dfn = Definition(OMMOD(mpath), name, targets.toList, no, localSection)
              sref.foreach(ref => SourceRef.update(dfn, ref))
              add(dfn)
          }
        case "assertion" =>
          val name = getName(n, thy)
          val spath = thy.path.toMPath ? name
          parseNarrativeObject(n)(dpath, thy, errorCont) match {
            case None => //nothing to do
            case Some(no) =>
              val ass = Assertion(OMMOD(mpath), name, no, localSection)
              sref.foreach(ref => SourceRef.update(ass, ref))
              add(ass)
          }
        case "example" => //omdoc example -> immt flexiformal declaration
          val name = getName(n, thy)
          val targetsS = (n \ "@for").text.split(" ")
          val targets = targetsS map { s =>
            val id = s.substring(1)
            mpath ? id //assuming all examples are local
          }
          parseNarrativeObject(n)(dpath, thy, errorCont) match {
            case None => //nothing to do
            case Some(no) =>
              val ex = Example(OMMOD(mpath), name, targets.toList, no, localSection)
              sref.foreach(ref => SourceRef.update(ex, ref))
              add(ex)
          }
        case "proof" => //omdoc proof -> immt flexiformal declaration
          val name = getName(n, thy)
          val targetsS = (n \ "@for").text.split(" ")
          val targets = targetsS map { s =>
            val mp = parseMPath(s,dpath)
            controller.getO(mp) match {
              case Some(th:Theory) =>
                th.getConstants.head.path
              case Some(v : View) =>
                v.getDeclarations.filter(!_.isInstanceOf[Link]).head.path
              case _ =>
                throw Missing(LogicalDependency(mp))
            }
          }
          val no = translateCMP(rewriteCMP(n))(dpath, thy, errorCont: ErrorHandler)
          val prf = Proof(OMMOD(mpath), name, targets.toList, no, localSection)
          sref.foreach(ref => SourceRef.update(prf, ref))
          add(prf)
        case "exercise" =>
          val name = getName(n, thy)
          parseNarrativeObject(n)(dpath, thy, errorCont) match {
            case None => //nothing to do
            case Some(no) =>
              val prob = no
              val sol = n.child.find(_.label == "solution").flatMap(c => parseNarrativeObject(c)(dpath, thy, errorCont))
              val ex = Exercise(OMMOD(mpath), name, prob, sol, localSection)
              sref.foreach(ref => SourceRef.update(ex, ref))
              add(ex)
          }

        case "omtext" =>
          val name = getName(n, thy)
          parseNarrativeObject(n)(dpath, thy, errorCont) match {
            case Some(no) =>
              val dfn = PlainNarration(OMMOD(mpath), name, no, localSection)
              sref.foreach(ref => SourceRef.update(dfn, ref))
              add(dfn)
            case None => //nothing to do
          }
        case "notation" =>
          //getting symbol info
          val cd = (n \ "@cd").text
          val name = (n \ "@name").text
          val refPath = Path.parseM("?" + cd, NamespaceMap(dpath))
          val refName = refPath ? LocalName(name)
          //getting mathml rendering info
          val prototype = n.child.find(_.label == "prototype").get
          val renderings = n.child.filter(_.label == "rendering")
          val cO = try {
            Some(controller.memory.content.getConstant(refName, p => "Notation for nonexistent constant " + p))
          } catch {
            case e: NotFound => None
          }
          def getstex(elem: Node,attr:String) = elem \ ("@{http://kwarc.info/ns/sTeX}" + attr)
          cO match {
            case Some(c) =>
              //getting macro info
              try {
                val macro_name = getstex(n,"macro_name").text
                val nrArgs = getstex(n,"nargs").text.toInt
                val macroMk = Delim("\\" + macro_name)
                val notArgs = macroMk :: (0 until nrArgs).toList.flatMap(i => Delim("{") :: SimpArg(i + 1) :: Delim("}") :: Nil)
                val stexScope = NotationScope(None, "stex" :: "tex" :: Nil, 0)
                val texNotation = new TextNotation(Mixfix(notArgs), Precedence.integer(0), None, false, stexScope)
                c.notC.parsingDim.set(texNotation)
              } catch {
                case e: Exception =>
                  val err = STeXParseError.from(e, "Notation is missing latex macro information", Some(s"for symbol $cd?$name"), sref, Some(Level.Warning))
                  errorCont(err)
              }

              try {
                renderings foreach { rendering =>
                  val notation = makeNotation(prototype, rendering)(doc.path)
                  if (notation.markers.nonEmpty)
                    c.notC.presentationDim.set(notation)
                }
              } catch {
                case e: Exception =>
                  val err = STeXParseError.from(e, "Invalid notation rendering", Some(s"for symbol $cd?$name"), sref, None)
                  errorCont(err)
              }
            case None =>
              val err = new STeXParseError("Notation for nonexistent constant ", Some(s"for symbol $refName"), sref, None)
              errorCont(err)
          }
        case "metadata" => //TODO
        case "#PCDATA" => //ignore
        case "ul" => //possibly/usually structural list
          val nextLevel = n.child.filter(_.label == "li").flatMap(li => li.child)
          nextLevel.foreach(c => translateDeclaration(c, localSection))
        case "omgroup" | "theory" =>
          val name = getName(n, thy)
          val innerSect = localSection / name
          val level = if (n.label == "theory") ModuleLevel else SectionInModuleLevel
          val innerDoc = new Document(mpath.toDPath / innerSect, level, contentAncestor = Some(thy))
          //NarrativeMetadata.title.update(innerDoc, title)
          add(innerDoc)
          n.child.foreach(c => translateDeclaration(c, innerSect))
        case "oref" => //TODO should be a special transclusion reference for partial documents
          val href = (n \ "@href").text
          href.split("#").toList match {
            case "foo" :: id :: Nil => //transclusion ref
              val op = new opaque.UnknownOpaqueElement(mpath.toDPath, "mmt", n.child)
              add(op)
            case _ => //nothing to do
          }
        case "structure" =>
          val name = LocalName((n\"@name").text)
          val fromN = (n\"@from").text
          val from = Path.parseM(fromN,NamespaceMap(dpath)) //(doc.path.^ / (fromN.tail + ".omdoc")) ? fromN.tail
          val s = Structure(thy.toTerm,name,OMID(from), isImplicit = false, isTotal = false)
          log("Structure " + name + ": " + from)
          add(s)
          // n.child.foreach(translateDeclaration(_)(doc, s, errorCont))
        case "div" =>
          n.child.foreach(n => translateDeclaration(n,localSection)(doc,thy,errorCont))
        case "p" =>
          log("Parsing paragraph as plain narration")
          val name = getName(n, thy)
          val nr = PlainNarration(OMMOD(mpath), name, translateCMP(rewriteCMP(n)), localSection)
          add(nr)
        case _ =>
          log("Parsing " + n.label + " as plain narration")
          val name = getName(n, thy)
          val nr = PlainNarration(OMMOD(mpath), name, translateCMP(rewriteCMP(n)), localSection)
          add(nr)
      }
    } catch {
      case e: Exception =>
        val sref = parseSourceRef(n, doc.path)
        val err = STeXParseError.from(e, "Skipping declaration-level element `" + n.label + "` due to unexpected error", None, sref, None)
//      println(err.extraMessage)
//      println(err.getStackTraceString)
        errorCont(err)
    }
  }

  def parseNarrativeObject(n: scala.xml.Node)(implicit dpath: DPath, thy: ModuleOrLink, errorCont: ErrorHandler): Option[Term] = {
    val sref = parseSourceRef(n, dpath)
    implicit val mpath : MPath = thy.path.toMPath
    n.child.filter(_.label=="uses").foreach(c => rewriteCMP(c)(thy,errorCont))
    n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) =>
        val narrNode = <div class="CMP">
          {nodes}
        </div> //effectively treating CMP as a narrative div
      val cmp = translateCMP(rewriteCMP(narrNode))(dpath, thy, errorCont: ErrorHandler)
        Some(cmp)
      case None =>
        val tp = (n\"@type").toString()
        if (tp == "simple") {
          val narrNode = <div class="CMP">
            {n.child}
          </div> //effectively treating CMP as a narrative div
          val cmp = translateCMP(rewriteCMP(narrNode))(dpath, thy, errorCont: ErrorHandler)
          Some(cmp)
        } else {
          val err = new STeXParseError("No CMP in narrative object `" + n.label + "`", None, sref, Some(Level.Warning))
          errorCont(err)
          None
        }
    }
  }

  abstract class ProtoPlaceholder(val nr: Int)

  case class ProtoArg(override val nr: Int) extends ProtoPlaceholder(nr)

  case class ProtoVar(override val nr: Int) extends ProtoPlaceholder(nr)

  case class ProtoSub(override val nr: Int) extends ProtoPlaceholder(nr)


  def parsePrototype(protoBody: scala.xml.Node, inBinder: Boolean = false)(implicit dpath: DPath): (GlobalName, Map[String, ProtoPlaceholder]) = {
    val argMap: collection.mutable.Map[String, ProtoPlaceholder] = new collection.mutable.HashMap()
    var nextArgNumber = 1 //start
    val symName = protoBody.label match {
        case "OMA" | "OMBIND" =>
          val n = protoBody.child.head
          n.label match {
            case "OMS" | "OMA" | "OMBIND" =>
              val (spath, args) = parsePrototype(n, protoBody.label == "OMBIND")
              argMap ++= args
              nextArgNumber = argMap.size + 1
              //computing map of arg names to positions
              protoBody.child.tail foreach { p =>
                p.label match {
                  case "expr" | "exprlist" =>
                    val name = (p \ "@name").text
                    argMap(name) = if (inBinder) ProtoSub(nextArgNumber) else ProtoArg(nextArgNumber)
                    if (name != "arg" + nextArgNumber)
                      argMap("arg" + nextArgNumber) = if (inBinder) ProtoSub(nextArgNumber) else ProtoArg(nextArgNumber)
                    nextArgNumber += 1
                  case "OMBVAR" =>
                    p.child foreach { ch =>
                      val name = (ch \ "@name").text
                      argMap(name) = ProtoVar(nextArgNumber)
                      nextArgNumber += 1
                    }
                  case _ => throw ParseError("invalid prototype" + protoBody)
                }
              }
              spath
            case _ => throw ParseError("invalid prototype" + protoBody)
          }
        case "OMS" =>
          val cd = (protoBody \ "@cd").text
          val name = (protoBody \ "@name").text
          val refPath = Path.parseM("?" + cd, NamespaceMap(dpath))
          refPath ? LocalName(name)
        case _ => throw ParseError("invalid prototype" + protoBody)
      }
    symName -> argMap.toMap
  }

  def makeNotation(prototype: scala.xml.Node, rendering: scala.xml.Node)(implicit dpath: DPath): TextNotation = {
    val protoBody = scala.xml.Utility.trim(prototype).child.head
    val renderingChildren = scala.xml.Utility.trim(rendering).child.toList
    val (symName, argMap) = parsePrototype(protoBody)

    val markers = renderingChildren.flatMap(mk => parseRenderingMarkers(mk, argMap))
    val precedence = getPrecedence(rendering)
    val variant = (rendering \ "@ic").text match {
      case "" => None
      case s => Some(s)
    }
    val languages = "mathml" :: Nil
    val scope = NotationScope(variant, languages, 0)
    val notation = new TextNotation(Mixfix(markers), precedence, None, false, scope)
    notation
  }

  def getPrecedence(n: scala.xml.Node): Precedence = {
    val precS = (n \ "@precedence").text
    try {
      Precedence.integer(precS.toInt)
    } catch {
      case _: Throwable => precS match {
        case "posinfty" => Precedence.infinite
        case "neginfty" => Precedence.neginfinite
        case _ => Precedence.integer(0) //default
      }
    }
  }


  private def getChildren(node: scala.xml.Node, pos: List[Int] = Nil)(implicit hasProp: scala.xml.Node => Boolean): Seq[(Node, List[Int])] = {
    if (hasProp(node)) {
      List((node, pos.reverse))
    } else {
      node.child.zipWithIndex.flatMap(p => getChildren(p._1, p._2 :: pos)).toSeq
    }
  }

  def translateCMP(n: scala.xml.Node)(implicit dpath: DPath, thy: ModuleOrLink, errorCont: ErrorHandler): Term = {
    val ret = translateCMPInner(n)
    if (ret.length != 1) {
      FlexiformalNode(n, ret.zipWithIndex.map{case (t,i) => (t,List(i))})
    } else ret.head
  }
  private def translateCMPInner(n: scala.xml.Node)(implicit dpath: DPath, thy: ModuleOrLink, errorCont: ErrorHandler): List[Term] = {
    val sref = parseSourceRef(n, dpath)
    n.label match {
      case "definiendum" =>
        val cd = (n \ "@cd").text
        val name = (n \ "@name").text
        val refName = resolveSPath(Some(cd), name, thy.path.toMPath)(thy,errorCont)
        val term = OMS(refName)
        val narr = <span class="stex-ref"><b>{n.child}</b></span>
        val ref = FlexiformalRef(term, narr)
        try {
          val const = controller.memory.content.getConstant(refName)
          //creating notation
          var i = 1
          var markers: Seq[Marker] = Nil //representing the verbalization with args (if Any)
          var markersOp: Seq[Marker] = Nil //representing the verbalization with no args (default values for args)
          var hasArgs = false
          n.child.toList foreach { c => c.label match {
              case "OMOBJ" if c.child.length == 1 =>
                //should be stored as it is for current content
                //added as argument for verbalization notation
                //added as placeholder Delimiter for default (no-args) verbalization
                val obj = c.child.head
                hasArgs = true
                markers :+= SimpArg(i)
                i += 1
                val defaultDelim = obj.label match {
                  case "OMV" => Delim((obj \ "@name").text)
                  case _ => Delim("todo")
                }
                markersOp :+= defaultDelim
              case "OMOBJ" =>
                val err = new STeXParseError("Invalid Object", Some(s"more than one child"), sref, Some(Level.Warning))
                errorCont(err)
              case "meta" =>
              case _ =>
                val words = c.text.split(" ").filterNot(_.isEmpty)
                markers ++= words.map(Delim(_))
                markersOp ++= words.map(Delim(_))
            }
          }
          val prec = Precedence.integer(0)
          val verbScope = NotationScope(None, sTeX.getLanguage(thy.path).toList, 0)
          val not = TextNotation.fromMarkers(prec, None, block = false, verbScope)(markers: _*)
          const.notC.verbalizationDim.set(not)
          if (hasArgs) {
            val notOp = TextNotation.fromMarkers(prec, None, block = false, verbScope)(markersOp: _*)
            const.notC.verbalizationDim.set(not)
          }
          val doc = controller.getDocument(const.parent.parent / (const.parent.name + ".omdoc"), d => "cannot find parent doc for reindexing" + d)
          importer.docCont.getOrElse(doc.path,importer.docCont(dpath / (thy.name + ".omdoc")))(doc) //reindexing that document
        } catch {
          case e: NotFound =>
            val err = new STeXParseError("Cannot add verbalization notation, symbol not found", Some(s"was looking for symbol $refName"), sref, Some(Level.Warning))
            errorCont(err)
          case e: GetError =>
            val err = new STeXParseError("Cannot add verbalization notation, symbol not found", Some(s"was looking for symbol $refName"), sref, Some(Level.Warning))
            errorCont(err)
          case e: NoSuchElementException =>
            val err = new STeXParseError("Cannot add verbalization notation, signature document not found for reindexing",
                                          Some(s"was looking for sig. document $dpath"), sref, Some(Level.Warning))
            errorCont(err)
        }
        ref :: Nil
        /*
      case "term" if (n \ "@class").text == "dangling-term-link" =>
        if (n.child.length == 1) translateCMP(n.child.head) :: Nil
        else {val err = new STeXParseError("dangling term link with more than one child",
          Some(s"was looking for sig. document $dpath"), sref, Some(Level.Warning))
          errorCont(err)
          makeNarrativeText("error")
        } :: Nil */
      case "term" =>
        val cd = (n \ "@cd").text
        val name = (n \ "@name").text
        (if (name == "") {
          translateCMP(<span class="stex-ref">{n.child}</span>)
        } else {
          val refName = resolveSPath(Some(cd), name, thy.path.toMPath)(thy, errorCont)
          val term = OMS(refName)
          val narr = <span class="stex-ref">
            {n.child}
          </span>
          FlexiformalRef(term, narr)
        }) :: Nil
      case "#PCDATA" =>
        if (n.toString().trim == "") Nil else
          makeNarrativeText(n.toString()) :: Nil
      case "OMOBJ" =>
        try { FlexiformalTerm(Obj.parseTerm(n, NamespaceMap(dpath))) :: Nil } catch {
          case t: Throwable =>
            ???
        }
      case "div" =>
        n.child.flatMap(c => translateCMPInner(c)).toList
      case _ => //informal (narrative) term
        val terms = getChildren(n)(n => n.label == "definiendum" || n.label == "term" || n.label == "OMOBJ").map(p => (translateCMP(p._1), p._2))
        FlexiformalNode(n, terms.toList) :: Nil
    }
  }

  def makeNarrativeText(s: String): Term = {
    val elems = s.toCharArray.toList
    def makeChildren(elems: List[Char], buffer: String): List[Node] = elems match {
      case Nil if buffer == "" => Nil
      case Nil => scala.xml.Text(buffer) :: Nil
      case hd :: tl if hd == ' ' =>
        scala.xml.Text(buffer) :: scala.xml.EntityRef("#160") :: makeChildren(tl, "")
      case hd :: tl => makeChildren(tl, buffer + hd)
    }

    val children = makeChildren(elems, "")
    FlexiformalXML(<span type="XML">{children}</span>)
    //TODO code above is unused, remove or use
    FlexiformalXML(scala.xml.Text(s))
  }

  def rewriteCMP(node: scala.xml.Node)(implicit thy: ModuleOrLink, errorCont: ErrorHandler): scala.xml.Node = {
    val mpath = thy.modulePath
    node.label match {
      case "OMS" if xml.attr(node, "cd") == "OMPres" && xml.attr(node, "name") == "PMML" =>
        MathMLNarration.term.toNode
      case "OMS" =>
        val cd = xml.attr(node, "cd")
        val name = xml.attr(node, "name")
        val sym = resolveSPath(Some(cd), name, mpath)(thy,errorCont)
          <om:OMS base={sym.module.parent.toPath} module={sym.module.name.last.toPath} name={sym.name.last.toPath}/>
      case "OME" => //OME(args) -> OMA(Informal.error -> args)
        val pre = OMS(Informal.constant("error")).toNode
        val newChild = node.child.map(rewriteCMP)
        new Elem(node.prefix, "OMA", node.attributes, node.scope, false, pre +: newChild: _*)
      case "OMATTR" => // FOR MIXING paper, to be updated when latexml bug is fixed
        try {
          val trimmed = scala.xml.Utility.trim(node)
          val omforeign = trimmed.child.head.child(1) // OMATP -> OMFOREIGN
          val refNr = (omforeign.child.head \ "@href").text
          require(omforeign.label == "FOREIGN", "attr value  should be OMFOREIGN")
          <om:OMV name={"mixref" + refNr}> </om:OMV>
        } catch {
          case e : Exception =>
            node
        }
      case "#PCDATA" => new scala.xml.Text(node.toString())
      //    case "OMATTR" if node.child(1).label == "OMV" => //Complex Variable
      //      val n = node.child(0).child(1) //TODO
      //      val nn = <om:OMFOREIGN>{scala.xml.Utility.trim(n).child.map(cleanNamespaces)}</om:OMFOREIGN>
      //      nn //TODO refactor cleanNamespaces to some xml helper object --duplicate in FlexiformalDeclarationObject
      case "uses" | "imports" =>
        val doc = controller.getAs(classOf[Document],mpath.parent / (mpath.name.toString + ".omdoc"))
        val thy = controller.getTheory(mpath)
        translateDeclaration(node)(doc,thy,errorCont)
        new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteCMP(_)(thy,errorCont)): _*)
      case _ =>
        new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteCMP(_)(thy,errorCont)): _*)
    }
  }


  def cleanNamespaces(node: scala.xml.Node): Node = node match {
    case el: Elem =>
      val scope = _cleanNamespaces(el.scope, Nil)
      new scala.xml.Elem(null, el.label, el.attributes, scope, el.minimizeEmpty, el.child.map(cleanNamespaces): _*)
    case _ => node
  }

  private def _cleanNamespaces(scope: NamespaceBinding, prefixes: List[String] = Nil): NamespaceBinding = {
    if (scope == scala.xml.TopScope) {
      scope
    } else {
      if (prefixes.contains(scope.prefix)) {
        _cleanNamespaces(scope.parent, prefixes)
      } else {
        NamespaceBinding(scope.prefix, scope.uri, _cleanNamespaces(scope.parent, scope.prefix :: prefixes))
      }
    }
  }


  def parseRenderingMarkers(n: scala.xml.Node, argMap: Map[String, ProtoPlaceholder]): List[Marker] = n.label match {
    case "menclose" =>
      assert(n.child.length == 1)
      parseRenderingMarkers(n.child.head,argMap)
    case "mspace" =>
      Delim(" ") :: Nil
    case "mrow" => Delim("(") :: n.child.flatMap(parseRenderingMarkers(_, argMap)).toList ::: List(Delim(")"))
    case "mmultiscripts" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated as mrow because not sure what it should do
    case "msub" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val sub = parseRenderingMarkers(n.child(1), argMap)
      main ::: List(Delim("_")) ::: sub
    case "msup" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val sup = parseRenderingMarkers(n.child(1), argMap)
      main ::: List(Delim("^")) ::: sup
    case "msubsup" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val sub = parseRenderingMarkers(n.child(1), argMap)
      val sup = parseRenderingMarkers(n.child(2), argMap)
      main ::: List(Delim("_")) ::: sub ::: List(Delim("^")) ::: sup
    case "munder" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val under = parseRenderingMarkers(n.child(1), argMap)
      main ::: List(Delim("__")) ::: under
    case "mover" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val over = parseRenderingMarkers(n.child(1), argMap)
      main ::: List(Delim("^^")) ::: over
    case "munderover" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0), argMap)
      val under = parseRenderingMarkers(n.child(1), argMap)
      val over = parseRenderingMarkers(n.child(2), argMap)
      main ::: List(Delim("__")) ::: under ::: List(Delim("^^")) ::: over
    case "mpadded" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mo" => makeDelim(n.child.mkString) :: Nil
    case "mi" => makeDelim("#id_" + n.child.mkString) :: Nil
    case "mn" => makeDelim("#num_" + n.child.mkString) :: Nil
    case "mtext" => makeDelim(n.child.mkString) :: Nil
    case "text" => makeDelim(n.child.mkString) :: Nil
    case "mfrac" => //assuming well formed elem and children
      val enum = parseRenderingMarkers(n.child(0), argMap)
      val denum = parseRenderingMarkers(n.child(1), argMap)
      enum ::: List(Delim("/")) ::: denum
    case "mtd" => val content = n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
      makeDelim("[&") :: content ::: makeDelim("&]") :: Nil
    case "mtr" => makeDelim("[\\") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: makeDelim("\\]") :: Nil
    case "mtable" => makeDelim("[[") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: makeDelim("]]") :: Nil
    case "render" =>
      val argName = (n \ "@name").text
      import CommonMarkerProperties.noProps
      argMap.getOrElse(argName,argName match { // TODO hack
        case "arg1" => argMap("args")
        case _ =>
          ???
      }) match {
        case ProtoArg(nr) => SimpArg(nr, noProps) :: Nil //TODO add precedence back and fix printing and parsing of Args with precedence Some(getPrecedence(n))
        case ProtoVar(nr) => Var(nr, typed = false, None, noProps) :: Nil //TODO Some(getPrecedence(n)
        case ProtoSub(nr) => SimpArg(nr, noProps) :: Nil //Some(getPrecedence(n))
      }
    case "iterate" =>
      val argName = (n \ "@name").text
      val argNr = if (argMap.isDefinedAt(argName)) argMap(argName) else if ((argName == "args" || argName == "arg") && argMap.isDefinedAt("arg1")) argMap("arg1") else ???
      val precO = None //Some(getPrecedence(n))
      val props = CommonMarkerProperties.noProps.copy(precedence = precO)
      val delim = n.child.find(_.label == "separator") match {
        case None => makeDelim(",")
        case Some(sep) =>
          sep.child.toList match {
            case Nil => makeDelim(",")
            case hd :: tl =>
              val dm = parseRenderingMarkers(hd, argMap).mkString("")
              makeDelim(dm)
          }
      }
      argNr match {
        case ProtoArg(nr) => SimpSeqArg(nr, delim, props) :: Nil
        case ProtoVar(nr) => Var(nr, typed = false, Some(delim), props) :: Nil
        case ProtoSub(nr) => throw new STeXParseError("Cannot have sequence sub as argument in notation rendering", None, None, None)
      }
    case "mstyle" => n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "merror" => Nil
    case "mroot" => Delim("√") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "msqrt" => //SqrtMarker(n.child.toList.flatMap(parseRenderingMarkers(_, argMap))) :: Nil
      Delim("√") :: Delim("(") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: List(Delim(")"))
    case "none" => Nil
  }

  def makeDelim(s: String): Delim = {
    val str = s.replaceAll("…", "...")
    str match {
      case "(" => Delim("&#40;")
      case ")" => Delim("&#41;")
      case _ => Delim(str)
    }
  }

  def parseSourceRef(n: scala.xml.Node, dpath: DPath)(implicit errorCont: ErrorHandler): Option[SourceRef] = {
    val attrs = n.attributes.asAttrMap
    if (attrs.contains("stex:srcref")) {
      //try to parse the source ref
      try {
        val srcrefS = n.attributes.asAttrMap("stex:srcref")
        val trangeIdx = srcrefS.indexOf("#textrange") + "#textrange".length
        val trangeS = srcrefS.substring(trangeIdx)
        val fromto = trangeS.split(",").toList
        fromto match {
          //(from=4;1,to=12;16)
          case fromS :: toS :: Nil =>
            val frangeIdx = fromS.indexOf("from=") + "from=".length
            val frangeS = fromS.substring(frangeIdx)
            val fvalsS = frangeS.split(";").toList
            val (fl, fr) = fvalsS match {
              case lS :: rS :: Nil =>
                val l = lS.toInt
                val r = rS.toInt
                (l, r)
              case _ => throw new STeXParseError("Invalid 'from' value in STeX source reference", Some(s"srcref value is `$srcrefS`"), None, None)
            }
            val trangeS = toS.substring("to=".length, toS.length - 1) //removing "to=" and ending bracket
          val tvalsS = trangeS.split(";").toList
            val (tl, tr) = tvalsS match {
              case lS :: rS :: Nil =>
                val l = lS.toInt
                val r = rS.toInt
                (l, r)
              case _ => throw new STeXParseError("Invalid 'to' value in STeX source reference ", Some(s"srcref value is `$srcrefS`"), None, None)
            }

            val from = SourcePosition(-1, fl, fr)
            val to = SourcePosition(-1, tl, tr)
            val sreg = SourceRegion(from, to)
            Some(SourceRef(dpath.uri, sreg))
          case _ => throw new STeXParseError("Invalid STeX source reference", Some(s"srcref value is `$srcrefS`"), None, None)
        }
      } catch {
        case e: STeXParseError => //reporting and returning none
          errorCont(e)
          None
        case e: Exception => //producing parse error and returning none
          val err = STeXParseError.from(e, "Failed to parse SourceRef", Some("for <" + n.label + " " + n.attributes.toString + ">"), None, Some(Level.Warning))
          errorCont(err)
          None
      }
    } else {
      //no srcref attr so returning none and producing an Info type error if actual node elem
      if (n.isInstanceOf[Elem]) {
        log("No stex:srcref attribute for element `" + n.label + "`")
        // val err = new STeXParseError("No stex:srcref attribute for element `" + n.label + "`", Some("while processing <" + n.label + " " + n.attributes.toString + ">"), None, Some(Level.Info))
        // errorCont(err)
      }
      None
    }
  }

  def parseRelDPath(s: String, dpath: DPath): DPath = {
    val (group, archive, frags)= parseStexPath(s, dpath)
    val base = mhBase / group / archive
    frags.foldLeft(base)((dc, x) => dc / x)
  }

  def parseMPath(s: String, base: DPath): MPath = {
    val parts = s.split("#")
    parts.toList match {
      case "" :: tnameS :: Nil => //document-local import
        base ? LocalName(tnameS)
      case fpathS :: tnameS :: Nil =>
        val dpath = parseRelDPath(fpathS, base)
        dpath ? LocalName(tnameS)
      case _ => throw new STeXParseError("Expected 2 `#` separated parts for module path", Some(" in: " + s), None, None)
    }
  }
  private def getIncludes(mp:MPath) : List[MPath] = {
    var dones : List[MPath] = Nil
    def getIncludesInner(imp : MPath) : Unit = if (!dones.contains(imp)) {
      dones ::= imp
      Try(controller.get(imp)).toOption match {
        case None =>
          throw Missing(LogicalDependency(imp))
        case Some(th: Theory) =>
          th.getIncludes.foreach(getIncludesInner)
          th.getNamedStructures.foreach(s => getIncludesInner(s.from.toMPath)) // TODO
        case Some(s: Structure) => // TODO
          getIncludesInner(s.parent)
          getIncludesInner(s.from.toMPath)
          s.getAllIncludes.map(i => getIncludesInner(i.from))
        case Some(v: View) =>
          getIncludesInner(v.from.toMPath)
          getIncludesInner(v.to.toMPath)
          v.getAllIncludes.map(i => getIncludesInner(i.from))
        case _ =>
          ???
      }
    }
    getIncludesInner(mp)
    dones
  }

  def resolveSPath(tnameSOp: Option[String], snameS: String, container: MPath)(thy: ModuleOrLink, errorCont: ErrorHandler): GlobalName = {
    val defaultDoc = container.doc
    val tnameSO = tnameSOp match {
      case Some("theory1") => Some(container.name.toString.split('.').head)
      case _ => tnameSOp
    }
    val defaultThy = tnameSO.fold(container.name)(LocalName(_))
    val defaultSym = LocalName(snameS)
    val tpaths =  getIncludes(container)
    // val tpaths = getIncludes(container)//controller.globalLookup.visible(OMMOD(container)) //includes container
    //filter those that match tname
    val thyOptions = tpaths.map(_.toMPath).filter(t => tnameSO.fold(true)(t.name.last.toPath == _)).distinct
    val spath = thyOptions match {
      case Nil => // taking dpath from container
        if (tnameSO.contains("ambiguous") || tnameSO.contains("latexml") || tnameSO.contains("unknown")) {
          log("ambiguous: " + (defaultDoc ? defaultThy ? defaultSym))
        } else {
          val err = new STeXLookupError("Cannot resolve module for symbol path, using default values", Some("looking for " + tnameSO.getOrElse("*") + "?" + snameS +
            " from theory " + container.toPath + tpaths.mkString("[", ",", "]")), Some(Level.Warning)) //perhaps not included
          errorCont(err)
        }
        defaultDoc ? defaultThy ? defaultSym
      case hd :: Nil => // found one matching thy, will use as default
        hd ? defaultSym
      case l => //several options, must look inside theories to resolve
        val thys = l flatMap { p =>
          try {
            Some(controller.get(p))
          } catch {
            case e: Error =>
              throw Missing(LogicalDependency(p))
              Nil
          }
        }

        val symOptions = thys flatMap {
          case d: Theory =>
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => Nil
        }
        symOptions match {
          case Nil => //adding error and defaulting
            val err = new STeXLookupError("Cannot resolve symbol path, symbol does not exist. Using default values", Some(" looking for module=" + tnameSO.getOrElse("*") +
              " and symbol=" + snameS + ". No matching modules " + l + " , contain symbol " + snameS), Some(Level.Warning))
            errorCont(err)
            defaultDoc ? defaultThy ? defaultSym
          case hd :: Nil => hd.path
          case _ => //adding error and defaulting
            // one of them is probably the current definition
            val others = symOptions.filter(_.parent != thy.modulePath)
            if (others.length == 1) others.head.path else {
              val err = new STeXLookupError("Cannot resolve symbol path, symbol does not exist. Using default values", Some(" looking for module=" + tnameSO.getOrElse("*") +
              " and symbol=" + snameS + ". No matching modules " + l + " , contain symbol " + snameS), Some(Level.Warning))
              errorCont(err)
              others.head.path
            }
        }
    }
    spath
  }
}

 */
