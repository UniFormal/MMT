package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import utils._
import documents._
import uom._
import presentation._

import scala.xml.Utility

abstract class MWSExporter extends Exporter {
  private lazy val pmmml = controller.extman.get(classOf[ParallelMathMLPresenter]).headOption.getOrElse({
    val e = new ParallelMathMLPresenter
    controller.extman.addExtension(e)
    e
  })

  def exportTheory(t: Theory, bf: BuildTask) {
    try {controller.simplifier(t)} catch { case _ => /* am besten alles */ }

    rh(xml.header)

    rh(xml.openTag("mws:harvest", List(("xmlns:mws", xml.namespace("mws")), ("xmlns:m", xml.namespace("mathml")))) + "\n")

    val data_id = "1"
    rh(xml.openTag("mws:data", List(("mws:data_id", data_id))))

    // id is the MMT URI
    rh(<id>{t.path.toPath}</id>.toString + "\n")

    // TODO: add "text" inside the theory, perhaps comments or other things?
    rh(<text></text>.toString + "\n")
    rh(<metadata></metadata>.toString + "\n")

    t.getDeclarationsElaborated foreach {d =>
      d.getComponents.foreach {
        case DeclarationComponent(comp, tc: AbstractTermContainer) =>
          tc.get.foreach {t =>
            val url = CPath(d.path,comp)
            val mathml = pmmml.asXML(t, Some(url))

            rh(xml.openTag("math", List(("local_id", url.toPath))) + "\n")
            rh(Utility.escape(mathml.toString) + "\n")
            rh(xml.closeTag("math"))
          }
        case _ =>
      }
    }

    rh(xml.closeTag("mws:data") + "\n")

    t.getDeclarationsElaborated foreach {d =>
      d.getComponents.foreach {
        case DeclarationComponent(comp, tc: AbstractTermContainer) =>
          tc.get.foreach {t =>
            val url = CPath(d.path,comp)

            rh(xml.openTag("mws:expr", List(("url", url.toPath), ("mws:data_id", data_id))))
            rh(ContentMathMLPresenter(t).toString)
            rh(xml.closeTag("mws:expr"))
            rh("\n")
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

class MWSHarvestExporter extends MWSExporter {
  val key = "mws"
  override val outExt = "harvest"
}

class FlatteningMWSExporter extends MWSExporter {
  override val outDim = Dim("export", "mws-flat")
  val key = "mws-flat-harvest"
  override val outExt = "harvest"
  lazy val mf = controller.simplifier

  override def exportTheory(t: Theory, bf: BuildTask): Unit = {
    mf(t)
    super.exportTheory(t, bf)
  }
}


class FlatteningPresenter extends Presenter(new ParallelMathMLPresenter) {
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
        val newThys = List(thy) // Mihnea used to call enrichment here, which added declarations induced by views to theories
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
