package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import notations._
import frontend._
import backend._
import objects._
import utils._
import documents._
import parser._
import uom._

import scala.collection.immutable.{HashMap}
import scala.xml.Node

class MWSHarvestExporter extends Exporter {
  val key = "mws"
  override val outExt = "harvest"

  def exportTheory(t: Theory, bf: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {d =>
      d.getComponents.foreach {
         case DeclarationComponent(comp, tc: AbstractTermContainer) =>
            tc.get.foreach {t =>
               val node = <mws:expr url={CPath(d.path,comp).toPath}>{t.toCML}</mws:expr>
               rh(node.toString + "\n")
            }
         case _ =>
      }
    }
    rh("</mws:harvest>\n")
  }

  def exportView(v: View, bf: BuildTask) {
    //excluding expressions from views for now
  }


  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    //Nothing to do - MathML in namespaces
  }

  def exportDocument(doc : Document, bt: BuildTask) {
    //Nothing to do - no MathML at document level
  }
}




class FlatteningMWSExporter extends Exporter {
  override val outDim = Dim("export", "mws-flat")
  val key = "mws-flat-harvest"
  override val outExt = "harvest"
  lazy val mf = controller.simplifier
  def exportTheory(t : Theory, bd : BuildTask) {
    mf(t)
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {
      case d => d.getComponents.foreach {
         case DeclarationComponent(comp, tc: AbstractTermContainer) =>
            tc.get.foreach {t =>
               val node = <mws:expr url={CPath(d.path,comp).toPath}>{t.toCML}</mws:expr>
               rh(node.toString + "\n")
            }
         case _ =>
      }
    }
    rh("</mws:harvest>\n")

  }
    def exportView(v: View, bd: BuildTask) {
    //excluding expressions from views for now
  }

  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    //Nothing to do - MathML in namespaces
  }

  def exportDocument(doc : Document, bt: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      doc.getModules(controller.globalLookup) collect {
        case _ =>
      }
    } catch {
      case e : GetError => //doc not found, can ignore
    }
    rh("</mws:harvest>\n")
  }

}

import presentation._
import utils.xml._

class IDMathMLPresenter extends MathMLPresenter {
   override def doToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) {
      val nsAtts = List("xmlns" -> namespace("mathml"), "xmlns:jobad" -> namespace("jobad"))
      val mmtAtts = pc.owner match {
         case None => Nil
         case Some(cp) => List("jobad:owner" -> cp.parent.toPath, "jobad:component" -> cp.component.toString, "jobad:mmtref" -> "")
      }
      val idAtt = ( "id" -> o.hashCode.toString)
      // <mstyle displaystyle="true">
      pc.out(openTag("math",  idAtt :: nsAtts ::: mmtAtts))
      pc.out(openTag("semantics", Nil))
      body
      pc.out(openTag("annotation-xml", List("encoding" -> "MathML-Content")))
      pc.out(o.toCML.toString)
      pc.out(closeTag("annotation-xml"))
      pc.out(closeTag("semantics"))
      pc.out(closeTag("math"))
   }
}


import metadata._

class FlatteningPresenter extends Presenter(new IDMathMLPresenter) {
  def key: String = "flatmws"
  override val outExt = "html"
  lazy val mf = controller.extman.get(classOf[ElaborationBasedSimplifier]).head
  def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
    this._rh = rh
    val f = rh match {
      case fw : FileWriter => fw.filename
    }
    val folder = File(f.toJava.getParentFile())
    s match {
      case doc : Document =>
        wrapScope(standalone, doc.path)(doDocument(doc))
      case thy : Theory =>
        val newThys = if (thy.path.toPath.contains("math")) mf.enrichFineGrained(thy) else List(thy)
        newThys foreach { t =>
          val out = (folder / t.name.toPath).setExtension("html")
          this.outputTo(out) {
            wrapScope(standalone, thy.path)(doTheory(t))
          }
        }
      case view : View =>
        wrapScope(standalone, view.path)(doView(view))
      case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
    }
    //TODO? reset this._rh
  }
  protected val htmlRh = utils.HTML(s => rh(s))
  import htmlRh._

  def doDocument(doc : Document) {
    //nothing to do
  }

  private def doTheory(thy : Theory) {
    div ("theory") {
      thy.getDeclarations foreach {
      case c : Constant =>
        div ("constant") {
          div ("body") {
            text(c.name.last.toPath)
            c.tp.foreach { o =>
              text(" : ")
              objectLevel(o, None)(rh)
            }
            c.df.foreach { o =>
              text(" = ")
              objectLevel(o, None)(rh)
            }
          }
          c.getOrigin match {
             case ByStructureSimplifier(t, OMID(view)) =>
               val origin = t.toMPath
               val path = c.home.toMPath
               p {
                 text {
                   " Induced statement found in " + path.toPath + ". "
                 }
                 text {
                   path.last + " is a " + origin.last  + " if we interpret over view " + view.last + ". "
                 }
                 text {
                   origin.last + " contains the statement " + c.name.last + "."
                 }
               }
             case _ =>
          }
        }
      case _ => //TODO
      }
    }
  }

  def doView(view : View) {//nothing to do

  }

  //utils
  def mathhubPath(p : Path) : String = {
    val uri = p.doc.uri
    //URI(uri.scheme, uri.authority, uri.path.head :: uri.path.tail.head :: "source" :: uri.path.tail.tail, uri.absolute).toString
    uri.toString
  }


  def getTitle(uri : Path) : String = uri match {
    case m : MPath => m.name.toPath
    case _ => uri.last
  }

  def wrapScope(standalone : Boolean, uri : Path)(content : => Unit) {
    if (standalone) {
      rh("<!DOCTYPE html>")
      html{
        head{
          rh(<meta name="mmturi" content={uri.toPath}></meta>)
          rh(<title> {getTitle(uri)} </title>)
          rh(<meta name="url" content={mathhubPath(uri)}></meta>)
        }
        body{
          div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                              "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
            content
          }
        }
      }
    } else {
      div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                          "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
        content
      }
    }
  }
}
