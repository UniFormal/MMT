package info.kwarc.mmt.tps
/*
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.flexiformal._

import scala.xml.{Node,Elem,NamespaceBinding}

abstract class TPSError(msg : String) extends Error(msg)

case class TPSParseError(msg : String, sref : Option[SourceRef]) extends TPSError(msg) {
  private def srefS = sref.map(_.region.toString).getOrElse("")
  override def toNode = <error type={this.getClass().toString} shortMsg={this.shortMsg} level={Level.Error.toString} sref={srefS}> {this.getStackTraceString} </error>
}

class TPSImporter extends Importer {
  val key : String = "tps-omdoc"
  override val logPrefix = "tpsimporter"
  def includeFile(name : String) : Boolean = name.endsWith(".omdoc") //generated omdoc

  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
   }

  def importDocument(bt : BuildTask, cont : Document => Unit) {
    try {
      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, true)
      val node : Node = cp.document()(0)
      src.close
      val errors = translateDocument(node)(bt.narrationDPath)
      val doc = controller.getDocument(bt.narrationDPath)
      if (errors != Nil) {
        log("Errors: " + errors.mkString("\n"))
      }
      errors foreach {e =>
        bt.errorCont(e)
      }
      cont(doc)
    } catch {
      case e : Throwable =>
        log("WARNING: Skipping article due to error: " + e.toString() + " \n" + e.getStackTraceString) //skipping declaration
    }
  }

  def add(s : StructuralElement) = {
    log("adding " + s.path.toPath)
    controller.add(s)
  }

  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val omdocNS = "http://omdoc.org/ns"
  val mhBase = DPath(URI("http://mathhub.info/"))

  def getName(n : Node, container : Content) : LocalName = {
    try {
      val nameS =  (n \ s"@{$xmlNS}id").text
      LocalName(nameS)
    } catch {
      case e : Exception => LocalName("D" + container.children.length.toString)
    }
  }

  /**
   * Translate a toplevel <omdoc> node
   */
  private def translateDocument(n : Node)(implicit dpath : DPath) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "omdoc" =>
          //creating document and implicit theory
          implicit val doc = new Document(dpath)
          controller.add(doc)
          //recursing into children
          errors ++= n.child.map(translateModule).flatten
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }

  private def translateModule(n : Node)(implicit doc : Document) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "theory" =>
          val name = getName(n, doc)
          val thy = new DeclaredTheory(doc.path, name, None)
          val ref = MRef(doc.path, thy.path, true)
          add(ref)
          add(thy)
          errors ++= n.child.map(translateDeclaration(_)(doc, thy)).flatten
        case _ => //ignoring for now
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }

  private def translateDeclaration(n : Node)(implicit doc : Document, thy : DeclaredTheory) : List[Error] = {
    var errors : List[Error] = Nil
    val dpath = doc.path
    val mpath = thy.path
    try {
      n.label match {
        case "assertion" | "symbol" => //found assertion
          val name =  getName(n, doc)
          val c = new FinalConstant(OMMOD(mpath), name, None, TermContainer(None), TermContainer(None), None, NotationContainer())
          add(c)
        case _ => //ignoring
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }


}
*/
