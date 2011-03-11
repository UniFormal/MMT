package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.SystemOutDocumentTarget
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.vocab.OWL2Datatype
import org.semanticweb.owlapi.vocab.OWLFacet

// This example shows how to create dataranges

object DataRanges1 {
	def main(args: Array[String]) {
			// OWLDataRange is the superclass of all data ranges in the OWL API.
	        // Data ranges are used as the types of literals, as the ranges for data properties,as filler for data reatrictions.
	
	        // Get hold of a manager to work with
	        val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager();
	        val factory : OWLDataFactory = manager.getOWLDataFactory();
	
	        // OWLDatatype represents named datatypes in OWL.  These are a bit like classes whose instances are data values
	
	        // OWLDatatype objects are obtained from a data factory.
	        // The OWL2Datatype enum defines built in OWL 2 Datatypes
	
	        // Get hold of the integer datatype
	         val integer : OWLDatatype = factory.getOWLDatatype(OWL2Datatype.XSD_INTEGER.getIRI());
	
	        // For common data types there are some convenience methods of OWLDataFactory.  
	        val integerDatatype : OWLDatatype = factory.getIntegerOWLDatatype();
	        val floatDatatype : OWLDatatype = factory.getFloatOWLDatatype();
	        val doubleDatatype : OWLDatatype = factory.getDoubleOWLDatatype();
	        val booleanDatatype : OWLDatatype = factory.getBooleanOWLDatatype();
	
	        // The top datatype (analgous to owl:Thing) is rdfs:Literal, which can be obtained from the data factory
	        val rdfsLiteral : OWLDatatype = factory.getTopDatatype();
	
	        // Custom data ranges can be built up from these basic datatypes.  For example, it is possible to
	        // restrict a datatype using facets from XML Schema Datatypes.  For example, lets create a data range
	        // that describes integers that are greater or equal to 18
	        // To do this, we restrict the xsd:integer datatype using the xsd:minInclusive facet with a value of 18.
	        // Get hold of a literal that is an integer value 18
	        val eighteen : OWLLiteral = factory.getOWLLiteral(18);
	        // Now create the restriction.  The OWLFacet enum provides an enumeration of the various facets that can be used
	        val integerGE18 : OWLDatatypeRestriction = factory.getOWLDatatypeRestriction(integer, OWLFacet.MIN_INCLUSIVE, eighteen);
	
	        // We could use this datatype in restriction, as the range of data properties etc.
	        // For example, if we want to restrict the range of the :hasAge data property to 18 or more
	        // we specify its range as this data range
	
	        val pm : PrefixManager = new DefaultPrefixManager("http://www.semanticweb.org/ontologies/dataranges#");
	        val hasAge : OWLDataProperty = factory.getOWLDataProperty(":hasAge", pm);
	        val rangeAxiom : OWLDataPropertyRangeAxiom = factory.getOWLDataPropertyRangeAxiom(hasAge, integerGE18);
	
	        val ontology : OWLOntology = manager.createOntology(IRI.create("http://www.semanticweb.org/ontologies/dataranges"));
	        // Add the range axiom to our ontology
	        manager.addAxiom(ontology, rangeAxiom);


	        // For creating datatype restrictions on integers or doubles there are some convenience methods on OWLDataFactory
	        // Create a data range of integers greater or equal to 60
	        val integerGE60 : OWLDatatypeRestriction = factory.getOWLDatatypeMinInclusiveRestriction(60);
	        // Create a data range of integers less than 16
	        val integerLT16 : OWLDatatypeRestriction = factory.getOWLDatatypeMaxExclusiveRestriction(18);
	
	        // In OWL 2 it is possible to represent the intersection, union and complement of data types
	        // For example, we could create a union of data ranges of the data range
	        // integer less than 16 or integer greater or equal to 60
	        val concessionaryAge : OWLDataUnionOf = factory.getOWLDataUnionOf(integerLT16, integerGE60);
	
	        // We can also coin names for custom data ranges.  To do this we use an OWLDatatypeDefintionAxiom
	        // Get hold of a named datarange (datatype) that will be used to assign a name to our above datatype
	        val concessionaryAgeDatatype : OWLDatatype = factory.getOWLDatatype(":ConcessionaryAge", pm);
	        // Now create a datatype definition axiom
	        val datatypeDef : OWLDatatypeDefinitionAxiom = factory.getOWLDatatypeDefinitionAxiom(concessionaryAgeDatatype, concessionaryAge);
	
	        // Add the definition to our ontology
	        manager.addAxiom(ontology, datatypeDef);
	
	        // Dump our ontology
	        manager.saveOntology(ontology, new SystemOutDocumentTarget());
	        

	}
}