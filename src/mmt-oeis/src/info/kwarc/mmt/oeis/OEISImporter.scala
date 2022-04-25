package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.stex.{OMDoc}
import informal._
import modules._
import notations._
import objects._
import symbols._
import info.kwarc.mmt.oeis.parser.DocumentParser

import scala.xml.Node

// ERRORS
abstract class OEISError(msg: String) extends Error(msg)

case class OEISSourceError(msg: String, sref: Option[SourceRef], severity: Option[Level.Level]) extends OEISError(msg) {
  private def srefS = sref.map(_.region.toString).getOrElse("")

  override def toNode = <error type={this.getClass.toString} shortMsg={this.shortMsg} level={severity.getOrElse(Level.Error).toString} sref={srefS}>
    {this.extraMessage}
  </error>
}

case class OEISLookupError(msg: String) extends OEISError(msg)

object OEISParseError {
  def apply(msg: String, sref: Option[SourceRef], severity: Option[Level.Level]): Error = sref match {
    case Some(ref) => new SourceError("oeis", ref, msg, Nil, severity.getOrElse(Level.Error))
    case None => OEISSourceError(msg, None, severity)
  }

  def from(e: Exception, preMsg: Option[String] = None, sref: Option[SourceRef] = None, severity: Option[Level.Level] = None): Error = {
    val pre = preMsg.map(_ + ": ").getOrElse("")
    val errMsg = pre + {
      e match {
        case er: Error => er.shortMsg
        case ex: Exception => ex.getMessage
      }
    }
    val err = apply(errMsg, sref, severity)
    err.setStackTrace(e.getStackTrace)
    err
  }
}

// IMPORTER
class OEISImporter extends Importer {
  val key: String = "oeis-omdoc"
  override val logPrefix = "oeisimporter"

  def inExts = List("txt")

  //stex/latexml generated omdoc
  var docParser: DocumentParser = null

  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
  }

  override def start(args: List[String]) {
    args match {
      case hd :: Nil =>
        val dict = scala.io.Source.fromFile(hd).getLines().map(_.trim).toSet
        docParser = new DocumentParser(dict)
      case _ => throw new Exception("Cannot initialize OEISImporter expected one argument (path to the dictionary file), found: " + args.toString)
    }
  }

  def parseSourceRef(n: scala.xml.Node, dpath: DPath)(implicit errorCont: ErrorHandler): Option[SourceRef] = {
    OMDoc.parseSourceRef(n, dpath)
  }

  def importDocument(bt: BuildTask, cont: Document => Unit): BuildResult = {
    try {
      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val text = try src.mkString finally src.close()
      src.close
      val newSrc = scala.io.Source.fromString(text)
      val node = docParser.fromReaderToXML(newSrc)
      //val cp = scala.xml.parsing.ConstructingParser.fromSource(src, true)
      //val node : Node = cp.document()(0)
      newSrc.close

      val defSRef = OMDoc.getDefaultSRef(text, bt.narrationDPath)
      val errHandler = new HandlerWithTreshold(bt.errorCont, Level.Warning)

      translateDocument(node, defSRef)(bt.narrationDPath, errHandler)
      val doc = controller.getDocument(bt.narrationDPath)
      cont(doc)
      controller.clear //to save memory from getting too high
    } catch {
      case e: Throwable =>
        log("WARNING: Skipping article due to error: " + e.toString + " \n" + e.getStackTrace.mkString("\n")) //skipping declaration
    }
    BuildResult.empty
  }


  def translateText(text: String)(implicit dpath: DPath, errCont: ErrorHandler): (Document, Node) = {
    val defSRef = OMDoc.getDefaultSRef(text, dpath)
    val src = scala.io.Source.fromString(text)
    val node = docParser.fromReaderToXML(src)
    src.close()
    translateDocument(node, defSRef)(dpath, errCont)
    val doc = controller.getDocument(dpath)
    (doc, node)
  }

  /**
    * Translate a toplevel <omdoc> node
    */
  private def translateDocument(n: Node, tsref: SourceRef)(implicit dpath: DPath, errorCont: ErrorHandler) {
    n.label match {
      case "omdoc" =>
        //creating document
        implicit val doc = new Document(dpath)
        controller.add(doc)
        //recursing into children
        n.child.foreach(n => translateModule(n, tsref))
    }
  }

  /**
    * translate second-level, in-document elements (typically modules)
    */
  private def translateModule(n: Node, tsref: SourceRef)(implicit doc: Document, errorCont: ErrorHandler) {
    val sref = OMDoc.parseSourceRef(n, doc.path).getOrElse(tsref)
    try {
      n.label match {
        case "theory" => //create theory
          val name = LocalName((n \ "@name").text)
          val thy = Theory.empty(doc.path, name, None)
          val ref = MRef(doc.path, thy.path)
          controller.add(ref)
          controller.add(thy)
          n.child.foreach(translateDeclaration(_, sref)(doc, thy, errorCont))
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
      }
    } catch {
      case e: Error => errorCont(e)
      case e: Exception => OEISParseError.from(e, Some("Skipping module-level element " + n.label + " due to error"), Some(sref), None)
    }
  }

  /**
    * translate third level, in-module elements (typically declarations)
    */
  private def translateDeclaration(n: Node, tsref: SourceRef)(implicit doc: Document, thy: Theory, errorCont: ErrorHandler) {
    val sref = OMDoc.parseSourceRef(n, doc.path).getOrElse(tsref)
    implicit val dpath = doc.path
    implicit val mpath = thy.path
    try {
      n.label match {
        case "assertion" =>
          val nr = thy.getDeclarations.length + 1
          val name = LocalName(n.label + nr)
          //val tpWrapperO = n.child.find(_.label == "type")
          val tpO = None //tpWrapperO.map(tpN => translateTerm(tpN.child.head))
        val dfO = None //TODO, get also def
        val const = Constant(OMMOD(mpath), name, Nil, TermContainer(tpO), TermContainer(dfO), None, NotationContainer())
          SourceRef.update(const, sref)
          controller.add(const)
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
        case "omtext" =>
          val nr = thy.getDeclarations.length + 1
          val name = LocalName(n.label + nr)
          parseNarrativeObject(n, sref) match {
            case Some(t) =>
              val dfn = PlainNarration(OMMOD(mpath), name, t, LocalName.empty)
              SourceRef.update(dfn, sref)
              controller.add(dfn)
            case _ => log("WARNING: Ignoring declaration due to no object " + n.toString)
          }
        case "p" if (n \ "@class").text == "formula" => //CMP inside
          val sname = LocalName("t" + thy.getDeclarations.length.toString)
          implicit val spath = mpath ? sname
          parseNarrativeObject(n, sref) match {
            case Some(fo) =>
              val a = Assertion(OMMOD(mpath), sname, fo, LocalName.empty)
              SourceRef.update(a, sref)
              controller.add(a)
            case _ => errorCont(OEISParseError("Ignoring declaration due to no object " + n.toString, Some(sref), Some(Level.Warning)))
          }
        case _ => errorCont(OEISParseError("Ignoring declaration: " + n.label, Some(sref), Some(Level.Warning)))
      }
    } catch {
      case e: Error => errorCont(e)
      case e: Exception => OEISParseError.from(e, Some("Skipping declaration-level element " + n.label + " due to error"), Some(sref), None)
    }
  }

  def parseNarrativeObject(n: scala.xml.Node, tsref: SourceRef)(implicit dpath: DPath, mpath: MPath, errorCont: ErrorHandler): Option[Term] = {
    OMDoc.parseNarrativeObject(n, tsref)(dpath, mpath, errorCont, resolveSPath)
  }

  def resolveSPath(baseO: Option[String], tnameSO: Option[String], snameS: String, container: MPath, tsref: SourceRef)(implicit errorCont: ErrorHandler): GlobalName = {
    val tNameDef = tnameSO.map(LocalName(_)).getOrElse(container.name)
    val sNameDef = LocalName(snameS)
    val docDef = baseO.map(b => Path.parseD(b, NamespaceMap.empty)).getOrElse(container.doc)
    val defaultPath = docDef ? tNameDef ? sNameDef
    val tpaths = controller.globalLookup.visible(OMMOD(container)) //includes container
    //filter those that match tname
    val options = tpaths.map(_.toMPath).filter(t => tnameSO.map(t.name.last.toPath == _).getOrElse(true))
    options.toList match {
      case Nil =>
        if (!((tnameSO.contains("arithmetics") || tnameSO.isEmpty) && Arithmetics.contains(snameS))) {
          errorCont(OEISParseError("Unknown symbol " + tnameSO.getOrElse("*") + "?" + snameS + ", assuming variable", Some(tsref), Some(Level.Warning)))
        }
        defaultPath
      case l =>
        val matches = l.map(controller.get(_)) flatMap {
          case d: Theory =>
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => None
        }
        matches match {
          case Nil =>
            errorCont(OEISParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS +
              ". No matching modules " + l + " , contain symbol " + snameS, Some(tsref), Some(Level.Warning)))
            defaultPath
          case hd :: Nil => hd.path
          case _ =>
            errorCont(OEISParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS +
              ". Several matching symbols: " + matches.map(_.path), Some(tsref), Some(Level.Warning)))
            defaultPath
        }
    }
  }
}


