package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.File
import java.net.URI

import scala.collection.mutable.Set 
import scala.collection.JavaConversions._ 
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.lf._
//import java.io.FileNotFoundException
//import java.io.IOException
//import java.net.UnknownHostException
//import java.util.Map 
import info.kwarc.mmt.api.objects._  
import info.kwarc.mmt.api.utils._
import scala.collection.immutable.List 

class Export (manager : OWLOntologyManager , controller : Controller) {
   
	/**
    * translates all theories in an MMT document to ontologies
    * @param doc the URI of the MMT document
    * @return the list of IRIs of the ontologies
    */
   def documentToOWL(doc: DPath) : List[IRI] = {  
	null   
   }
}

object Export {	
	
	
	def main(args: Array[String]) {
		val checker = new FoundChecker(DefaultFoundation)
		val report = new FileReport(new java.io.File("controller.log")) 
		val controller = new Controller(checker, report)
		controller.handle(ExecFile(new java.io.File("startup.mmt"))) 
		val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
		val exporter = new Export (manager, controller)
		/*	
			val source : File = new File(arg(0))
			val target : File = new File(arg(1))			
		*/
		//val file : File = new File("examples\\ex2.owl");
		val source : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\declarationAxiom.omdoc")		
		val target : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\declarationAxiomToOWL.owl") 
		
		val doc : DPath  = controller.read(source)
		
		def writeToFile(iri : IRI, trg : File) {
			val onto = manager.getOntology(iri)
			val file = new java.io.FileWriter(trg)
			val ontoTarget = new WriterDocumentTarget(file) 
			manager.saveOntology(onto, ontoTarget)
			file.close
		}
		
		val iris : List[IRI] = exporter.documentToOWL(doc)
		if (iris.length == 1) {
			writeToFile(iris.head, target)
		} else {
			target.mkdirs()
		    iris foreach {iri => writeToFile(iri, new File(target, Utils.IRILast(iri)))}
		}
	}
}
