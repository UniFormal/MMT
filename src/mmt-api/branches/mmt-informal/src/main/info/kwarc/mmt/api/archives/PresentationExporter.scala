package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import presentation._
import modules._
import documents._

/**
 * common code for turning a [[Presenter]] into an exporter
 * 
 * The Presenter must have been registered before and is identified by passing the format as an argument to the start method.
 * 
 * If no presenter is found, a [[StyleBasedPresenter]] is created.
 * 
 */
trait PresentationExporter extends Exporter {
    protected var format : String = null
    protected var presenter : Presenter = null
    lazy val key = "present" + "_" + inDim + "_" + format 
    lazy val outDim = Dim("presentation", inDim.toString, format)
    
    override def start(args: List[String]) {
       args.length match {
          case 1 =>
             format = args(0)
             presenter = controller.extman.getPresenter(format) getOrElse {
                throw LocalError("no presenter found for format " + format)
             }
           case 2 =>
             format = args(0)
             val nset = Path.parseM(args(1), controller.getBase)
             presenter = new StyleBasedPresenter(controller, nset) 
           case _ =>
             throw LocalError("wrong number of arguments, expected 1 or 2")
       }
    }
}

/**
 * This class turns a [[Presenter]] into a ContentExporter.
 */
class PresentationContentExporter extends ContentExporter with PresentationExporter {
    def doNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir, DPath)], modules: List[(BuildFile,MPath)]) {
       val doc = controller.getDocument(dpath)
       presenter(doc, rh)
    }

    def doTheory(t: DeclaredTheory, bf: BuildFile) {
       presenter(t, rh)
    }
    def doView(v: DeclaredView, bf: BuildFile) {
       presenter(v, rh)
    } 
}

/**
 * This class turns a [[Presenter]] into a NarrationExporter.
 */
class PresentationNarrationExporter extends NarrationExporter with PresentationExporter {
    def doDocument(doc : Document, bt: BuildTask) {
    	 presenter(doc, rh)
    }
}