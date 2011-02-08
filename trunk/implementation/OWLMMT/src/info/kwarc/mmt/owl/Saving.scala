package info.kwarc.mmt.owl

import org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
 	
import java.io.File

object Saving {

	def main(args: Array[String]) {
		
			// Get hold of an ontology manager
		    val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager();
		
		    // Let's load an ontology from the web.  We load the ontology from a document IRI
		    val documentIRI : IRI = IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl");
		    val pizzaOntology : OWLOntology = manager.loadOntologyFromOntologyDocument(documentIRI);
		    println("Loaded ontology: " + pizzaOntology);
		
		    // Now save a local copy of the ontology.  (Specify a path appropriate to your setup)
		    // val file : File = new File("/tmp/local.owl");
		    val file : File = new File("E:\\Fall10\\CompSem\\Project\\ex.owl");
		    manager.saveOntology(pizzaOntology, IRI.create(file.toURI()));
		
		    // By default ontologies are saved in the format from which they were loaded. 
		    // In this case the ontology was loaded from an rdf/xml file
		    // We can get information about the format of an ontology from its manager
		    val format : OWLOntologyFormat = manager.getOntologyFormat(pizzaOntology);
		    println("    format: " + format);
		
		    // We can save the ontology in a different format
		    // Lets save the ontology in owl/xml format
		    val owlxmlFormat : OWLXMLOntologyFormat = new OWLXMLOntologyFormat();
		    // Some ontology formats support prefix names and prefix IRIs.  
		    // In our case we loaded the pizza ontology from an rdf/xml format, which supports prefixes.  
		    // When we save the ontology in the new format we will copy the prefixes over so that
		    // we have nicely abbreviated IRIs in the new ontology document
		    if(format.isPrefixOWLOntologyFormat()) {
		                owlxmlFormat.copyPrefixesFrom(format.asPrefixOWLOntologyFormat());
		    }
		    manager.saveOntology(pizzaOntology, owlxmlFormat, IRI.create(file.toURI()));
		
		    // We can also dump an ontology to System.out by specifying a different OWLOntologyOutputTarget
		    // Note that we can write an ontology to a stream in a similar way using the StreamOutputTarget class
		    val documentTarget : OWLOntologyDocumentTarget = new SystemOutDocumentTarget();
		    // Try another format - The Manchester OWL Syntax
		    val manSyntaxFormat : ManchesterOWLSyntaxOntologyFormat = new ManchesterOWLSyntaxOntologyFormat();
		    if(format.isPrefixOWLOntologyFormat()) {
		                manSyntaxFormat.copyPrefixesFrom(format.asPrefixOWLOntologyFormat());
		    }
		    manager.saveOntology(pizzaOntology, manSyntaxFormat, new SystemOutDocumentTarget());
	
	
	
		
	}

}