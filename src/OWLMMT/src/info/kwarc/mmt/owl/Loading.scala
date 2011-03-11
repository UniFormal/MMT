package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.File
import java.net.URI

/*
import java.io.FileNotFoundException
import java.io.IOException
import java.net.UnknownHostException
import java.util.Map 
*/


object Loading {
	
	def main(args: Array[String]) {
     
		    // Get hold of an ontology manager
			val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager();
			
			// Let's load an ontology from the web
			val iri : IRI = IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl");
			val  pizzaOntology : OWLOntology  = manager.loadOntologyFromOntologyDocument(iri);
			println("Loaded ontology: " + pizzaOntology);
			
			// Remove the ontology so that we can load a local copy.
			manager.removeOntology(pizzaOntology);
			
			// We can also load ontologies from files.  Download the pizza ontology from
			// http://www.co-ode.org/ontologies/pizza/pizza.owl and put it somewhere on your hard drive
			// Create a file object that points to the local copy
			val file : File = new File("E:\\Fall10\\CompSem\\Project\\ex.owl");
			
			
			// Now load the local copy
			val localPizza : OWLOntology  = manager.loadOntologyFromOntologyDocument(file);
			println("Loaded ontology: " + localPizza);
			
			// We can always obtain the location where an ontology was loaded from
			val documentIRI : IRI  = manager.getOntologyDocumentIRI(localPizza);
			println("    from: " + documentIRI);
			
			// Remove the ontology again so we can reload it later
			manager.removeOntology(pizzaOntology);
		
/*
			// In cases where a local copy of one of more ontologies is used, an ontology IRI mapper can be used
			// to provide a redirection mechanism.  This means that ontologies can be loaded as if they were located
			// on the web.
			// In this example, we simply redirect the loading from http://www.co-ode.org/ontologies/pizza/pizza.owl
			// to our local copy above.
			 
			manager.addIRIMapper(new SimpleIRIMapper(iri, IRI.create(file)));
			// Load the ontology as if we were loading it from the web (from its ontology IRI)
			pizzaOntologyIRI : IRI = IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl");
			redirectedPizza : OWLOntology = manager.loadOntology(pizzaOntologyIRI);
			println("Loaded ontology: " + redirectedPizza);
			println("    from: " + manager.getOntologyDocumentIRI(redirectedPizza));
			
			// Note that when imports are loaded an ontology manager will be searched for mappings
*/
	}
}