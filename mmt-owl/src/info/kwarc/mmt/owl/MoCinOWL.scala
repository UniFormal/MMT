package info.kwarc.mmt.owl
import java.io.File
import java.net.URI
import info.kwarc.mmt.api._
import utils.FileConversion._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.moc._
import scala.xml._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.apibinding.OWLManager


  //take two owl files as inputs
  //add identifiers to them
  //translate them to mmt files
  //apply mmt diff to the mmt files
  //dependsOn
  //propFunc
  object MoCinOWL {
	def main(args: Array[String]) {
		val controllerOlder = new Controller
	    val controllerCurrent = new Controller

	    controllerOlder.handle(ExecFile(new java.io.File("startup.mmt")))
	    controllerCurrent.handle(ExecFile(new java.io.File("startup.mmt")))
	   
	    var olderVersion : File = null
	    var currentVersion : File = null
	   
	    if(args.length < 1) 
	    { println("USAGE: OLDFILE")  
	      exit
	    }
	
	    olderVersion = new File(args(0)) //has to be in the source folder
	    currentVersion = new File(args(1)) //this should not be in the source folder; elsewhere
	    //we need the older version in the source folder to be able to get dependency relations.
	    //two versions have the same ontology IRI, so they should not be in the source folder together, because we cannot compare them
	   
	   //transates older OWL document to MMT in compiled folder
	   val controllerBuild = new Controller
	   controllerBuild.handleLine("File build-test.mmt")
	   	     
	   //translates current OWL document to MMT
	   val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
	   val importer = new Import (manager, controllerCurrent)
       val ontology : OWLOntology  = manager.loadOntologyFromOntologyDocument(currentVersion)
	   val dpath : DPath = importer.ontologyToLF(ontology)
	  
	   val olderDoc : DPath  = controllerOlder.read(olderVersion)
       val currentDoc : DPath = controllerCurrent.read(currentVersion)
        
       var diff = Differ.diff(controllerOlder, controllerCurrent, olderDoc, currentDoc)
       
	   //propagate(diff)
	  		
	   
	   
	}
  
}