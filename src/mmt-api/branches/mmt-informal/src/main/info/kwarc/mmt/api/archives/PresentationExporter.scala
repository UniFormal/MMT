package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import presentation._
import modules._
import documents._

/**
 * This class turns a [[Presenter]] into a ContentExporter.
 * 
 * The Presenter must have been registered before and is identified by passing the format as an argument to the start method.
 * 
 * If no presenter is found, a [[StyleBasedPresenter]] is created.
 */
class PresentationContentExporter extends ContentExporter {
    private var format : String = null
    private var presenter : Presenter = null
    lazy val key = "present_" + format 
    lazy val outDim = "present_" + format
    
    override def start(args: List[String]) {
       args.length match {
          case 1 => 
       	    format = args(0)
       		presenter = controller.extman.getPresenter(format) getOrElse {throw LocalError("Presenter not found for format " + format)}
		 case 2 => 
         	val format = args(0)
			val nset = Path.parseM(args(1), controller.getBase)
         	new StyleBasedPresenter(controller, nset) 
		 case _ => 
		    throw LocalError("wrong number of arguments, expected 1 or 2")
       }
    }
    
    def doNamespace(dpath: DPath,namespaces: List[(BuildDir, DPath)], modules: List[(BuildFile,MPath)]) {}

    def doTheory(t: DeclaredTheory, bf: BuildFile) {
       presenter(t, rh)
    }
    def doView(v: DeclaredView, bf: BuildFile) {
       presenter(v, rh)
    } 
}

class PresentationNarrationExporter extends NarrationExporter {
    private var format : String = null
    private var presenter : Presenter = null
    lazy val key = "narration_present_" + format 
    lazy val outDim = "narration_present_" + format
    
    override def start(args: List[String]) {
       args.length match {
          case 1 => 
       	    format = args(0)
       		presenter = controller.extman.getPresenter(format) getOrElse {throw LocalError("Presenter not found for format " + format)}
		 case 2 => 
         	val format = args(0)
			val nset = Path.parseM(args(1), controller.getBase)
         	new StyleBasedPresenter(controller, nset) 
		 case _ => 
		    throw LocalError("wrong number of arguments, expected 1 or 2")
       }
    }
    
    def doDocument(doc : Document) {
    	presenter(doc, rh)
    }
}