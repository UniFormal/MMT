package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWLFacet

// This example shows how to work with dataranges.  OWL 1.1 allows data ranges to be created by taking a base datatype
// e.g. int, string etc. and then by applying facets to restrict the datarange. For example, int greater than 18

object DataRanges2 {
	def main(args: Array[String]) {
			val man : OWLOntologyManager = OWLManager.createOWLOntologyManager();
            val base : String = "http://org.semanticweb.datarangeexample";
            val ont : OWLOntology = man.createOntology(IRI.create(base));

            // We want to add an axiom to our ontology that states that adults
            // have an age greater than 18.  To do this, we will create a restriction
            // along a hasAge property, with a filler that corresponds to the set
            // of integers greater than 18.

            // First get a reference to our hasAge property
            val factory : OWLDataFactory = man.getOWLDataFactory();
            val hasAge : OWLDataProperty = factory.getOWLDataProperty(IRI.create(base + "hasAge"));
            // For completeness, we will make hasAge functional by adding an axiom to state this
            val funcAx : OWLFunctionalDataPropertyAxiom = factory.getOWLFunctionalDataPropertyAxiom(hasAge);
            man.applyChange(new AddAxiom(ont, funcAx));

            // Now create the data range which correponds to int greater than 18.  To do this, we
            // get hold of the int datatype and then restrict it with a minInclusive facet restriction.
            val intDatatype : OWLDatatype = factory.getIntegerOWLDatatype();
            // Create the value "18", which is an int.
            val eighteenConstant : OWLLiteral = factory.getOWLLiteral(18);
            // Now create our custom datarange, which is int greater than or equal to 18.  To do this,
            // we need the minInclusive facet
            val facet : OWLFacet = OWLFacet.MIN_INCLUSIVE;
            // Create the restricted data range by applying the facet restriction with a value of 18 to int
            val intGreaterThan18 : OWLDataRange = factory.getOWLDatatypeRestriction(intDatatype,
                    facet,
                    eighteenConstant);
            // Now we can use this in our datatype restriction on hasAge
            val thingsWithAgeGreaterOrEqualTo18 : OWLClassExpression = factory.getOWLDataSomeValuesFrom(hasAge, intGreaterThan18);
            // Now we want to say all adults have an age that is greater or equal to 18 - i.e. Adult is a subclass of
            // hasAge some int[>= 18]
            // Obtain a reference to the Adult class
            val adult : OWLClass = factory.getOWLClass(IRI.create(base + "#Adult"));
            // Now make adult a subclass of the things that have an age greater to or equal to 18
            val ax : OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(adult, thingsWithAgeGreaterOrEqualTo18);
            // Add our axiom to the ontology
            man.applyChange(new AddAxiom(ont, ax));


	
	}
}