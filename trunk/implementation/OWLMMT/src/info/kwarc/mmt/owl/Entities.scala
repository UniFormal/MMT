package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.DefaultPrefixManager

// This example shows how to get access to objects that represent entities
// Shows how to obtain references to entities (classes, properties, individuals etc.)
object Entities {

	def main(args: Array[String]) {
     
			// In order to get access to objects that represent entities we need a data factory.
			val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager();
			
			// We can get a reference to a data factory from an OWLOntologyManager.
			val factory : OWLDataFactory = manager.getOWLDataFactory();
			 
			// In OWL, entities are named objects that are used to build class expressions and axioms.
			// They include classes, properties (object, data and annotation), named individuals and datatypes.
			// All entities may be obtained from an OWLDataFactory.
			
			// Let's create an object to represent a class.  In this case, we'll choose
			// http://www.semanticweb.org/owlapi/ontologies/ontology#A as the IRI for our class.
			
			// There are two ways we can create classes (and other entities).
	
			// The first is by specifying the full IRI.  First we create an IRI object:
			val  iri : IRI = IRI.create("http://www.semanticweb.org/owlapi/ontologies/ontology#A");
			// Now we create the class
			val  clsAMethodA : OWLClass = factory.getOWLClass(iri);
	
	    	// The second is to use a prefix manager and specify abbreviated IRIs.  This is useful for creating
	        // lots of entities with the same prefix IRIs.
	        // First create our prefix manager and specify that the default prefix IRI (bound to the empty prefix name)
	        // is http://www.semanticweb.org/owlapi/ontologies/ontology#
	        val pm : PrefixManager = new DefaultPrefixManager("http://www.semanticweb.org/owlapi/ontologies/ontology#");
	        // Now we use the prefix manager and just specify an abbreviated IRI
	        val clsAMethodB : OWLClass = factory.getOWLClass(":A", pm);
	
	        // Note that clsAMethodA will be equal to clsAMethodB because they are both OWLClass objects and have the
	        // same IRI.
	
	        // Creating entities in the above manner does not "add them to an ontology".  They are merely objects that
	        // allow us to reference certain objects (classes etc.) for use in class expressions, and axioms (which can
	        // be added to an ontology).
	
	        // Lets create an ontology, and add a declaration axiom to the ontology that declares the above class
	        val ontology : OWLOntology = manager.createOntology(IRI.create("http://www.semanticweb.org/owlapi/ontologies/ontology"));
	        // We can add a declaration axiom to the ontology, that essentially adds the class to the signature of
	        // our ontology.
	        val  declarationAxiom : OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(clsAMethodA);
	        manager.addAxiom(ontology, declarationAxiom);
	        // Note that it isn't necessary to add declarations to an ontology in order to use an entity.  For some
	        // ontology formats (e.g. the Manchester Syntax), declarations will automatically be added in the saved
	        // version of the ontology.    	   
	    	   
    }
	
	
}