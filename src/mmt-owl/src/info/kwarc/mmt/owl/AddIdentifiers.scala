package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.SimpleIRIMapper
import java.io.{File,FileWriter}
import java.net.URI
import scala.collection.mutable.Set // 
import scala.collection.JavaConversions._ //

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.backend.{Compiler}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.objects._  //import jomdoc.objects.{Term,OMS,OMI}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.FileConversion._
import scala.collection.immutable.List //
import info.kwarc.mmt.api.metadata._

import uk.ac.manchester.cs.owl.owlapi._

//class AddIdentifiers (manager : OWLOntologyManager, controller : Controller)  {
class AddIdentifiers (managerID : OWLOntologyManager)  {  
  
  val omdocIRI : IRI = IRI.create("http://omdoc.org/identifier#id")
  private val dataFactory = new OWLDataFactoryImpl
  
  def ontologyToOntologyIDs(ontology : OWLOntology) : OWLOntology = {
    
	val ontoIRI : IRI  = ontology.getOntologyID.getOntologyIRI
	val ontologyIDs = managerID.createOntology(IRI.create(ontoIRI.toString))
	
	val axioms : List[OWLAxiom] = ontology.getAxioms.toList
	val (logicals,nonLogicals) = axioms.partition((a : OWLAxiom) => a.isLogicalAxiom)
	
	nonLogicals.foreach(axiom => managerID.addAxiom(ontologyIDs, axiom)) // declaration and annotataion axioms
	logicals.foreach(axiom => managerID.addAxiom(ontologyIDs, axiomToAxiomID(axiom)))

	ontologyIDs
  }
  
  def axiomToAxiomID (ax : OWLAxiom) : OWLAxiom = {
    var annotationsID = new java.util.HashSet(ax.getAnnotations)
    ax.getAnnotations.find(annot => annot.getProperty.getIRI == omdocIRI) match {
						case Some(annot) => 
						case None => annotationsID.add(createAnnotationID(ax))
     					}
    /*//to check datatypeDefinition axiom
      println(annotationsID)
      println(ax.getAnnotatedAxiom(annotationsID))
    */
    ax.getAnnotatedAxiom(annotationsID)
  }

  def createAnnotationID (ax : OWLAxiom) : OWLAnnotation = {
    val property : OWLAnnotationProperty = dataFactory.getOWLAnnotationProperty(omdocIRI)
    val value : OWLAnnotationValue = dataFactory.getOWLLiteral("ax" + ax.hashCode)
    dataFactory.getOWLAnnotation(property, value)
    
  }
      
}

object AddIdentifiers {
   def main(args: Array[String]) {
     
     val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
	 val managerID: OWLOntologyManager = OWLManager.createOWLOntologyManager()
     val addIdentifiers = new AddIdentifiers (managerID)
     
     val source : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\source\\Identifiers\\identifiersChanged.owl")		
	 val target: File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\source\\Identifiers\\identifiersChangedIDs.owl")
     
     val ontology : OWLOntology  = manager.loadOntologyFromOntologyDocument(source)
     println("Loaded Ontology: " + ontology)
     val ontologyIDs = addIdentifiers.ontologyToOntologyIDs(ontology)
     println("Ontology with IDs: " + ontologyIDs)
  		      
     val file = new java.io.FileWriter(target)
	 val ontologyTarget = new WriterDocumentTarget(file)
	 val owlxmlFormat : OWLXMLOntologyFormat = new OWLXMLOntologyFormat()
     managerID.saveOntology(ontologyIDs, owlxmlFormat, ontologyTarget)
	 file.close
   }

}