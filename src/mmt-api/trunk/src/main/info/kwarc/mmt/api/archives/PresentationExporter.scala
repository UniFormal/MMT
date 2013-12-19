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
trait PresentationExporter extends frontend.Extension {
    protected var format : String = null
    protected var presenter : Presenter = null
    def formatPrefix : String
    lazy val key = formatPrefix + format 
    lazy val outDim = formatPrefix + format
    
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
    val formatPrefix = "cont_present_"
    def doNamespace(dpath: DPath,namespaces: List[(BuildDir, DPath)], modules: List[(BuildFile,MPath)]) {}

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
    val formatPrefix = "narr_present_"
    def doDocument(doc : Document, bt: BuildTask) {
    	 presenter(doc, rh)
    }
}