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
import uk.ac.manchester.cs.owl.owlapi._
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.vocab.OWLFacet
 
class Export (manager : OWLOntologyManager , controller : Controller) {
    private var ontology : OWLOntology = null
    private val dataFactory = new OWLDataFactoryImpl
    
    def globalNameToIRI(gname : GlobalName) : IRI = {
        gname match {
	    	case OMMOD(doc ? !("_")) % name => IRI.create(doc.toPath + "/" + name.flat)
	    	case _ => IRI.create(gname.toPath)
        }
    }
        
    def modNameToIRI(m: MPath) : IRI = {
        if (m.name == LocalPath(List("_")))
	  	   IRI.create(m.doc.toPath)
	    else 
	  	   IRI.create(m.toPath)
    }
    
  	/**
    * translates all theories in an MMT document to ontologies
    * @param doc the URI of the MMT document
    * @return the list of IRIs of the ontologies
    */
    
   def documentToOWL (doc: DPath) : List[IRI] = {  
	   val document = controller.getDocument(doc)
	   val modules = document.getModulesResolved(controller.library)
	   modules.map { module => 
     				 module match {
  	   						case module : DeclaredTheory => theoryToOWL(module)
  	   						//case module : View
	     		     }
	   }
   } 
   
   def theoryToOWL (theory : DeclaredTheory) : IRI = { 
	   val ontoIRI = modNameToIRI(theory.path)
	   ontology = manager.createOntology(ontoIRI)
	   
	   val symbols = theory.valueListNG 
	   symbols.foreach {
	   symbol => symbol match {
	  	  	   			case symbol : Constant => constantToOWL(symbol)
	  	  	   		  //case symbol : Include => IncludeToOWL(symbol)
	   	   }
	   }
	   ontoIRI
   }
   
   def classToOWL (t: Term) : OWLClassExpression = {
	   t match {
	  	 case OMID(p) => dataFactory.getOWLClass(globalNameToIRI(p))
	  	 //case t : OMID => val p = t.gname
	 
    	 case OMA(OWL2OMS("OWL2SUB","objectIntersectionOf"), args) =>
	  	      val argsList = args.map(classToOWL)
	  	      dataFactory.getOWLObjectIntersectionOf(argsList.toSet)
	  	       	  	        
	  	 case OMA(OWL2OMS("OWL2QL", "objectUnionOf"),args) => 
	  	      val argsList = args.map(classToOWL) 
	  	      dataFactory.getOWLObjectUnionOf(argsList.toSet)

         case OMA(OWL2OMS("OWL2QL", "objectComplementOf"),args) =>     
	  	      val operand = classToOWL(args(0))
	  	      dataFactory.getOWLObjectComplementOf(operand) 
	  	        
         case OMA(OWL2OMS("OWL2QL","objectAllValuesFrom"),args) =>   
	  	      val property = propertyToOWL(args(0))
	  	      val filler = classToOWL(args(1))
	  	      dataFactory.getOWLObjectAllValuesFrom(property, filler)
	  	 
         case OMA(OWL2OMS("OWL2EL","objectSomeValuesFrom"),args) =>   
	  	      val property = propertyToOWL(args(0))
	  	      val filler = classToOWL(args(1))
	  	      dataFactory.getOWLObjectSomeValuesFrom(property, filler)
  	        
         case OMA(OWL2OMS("OWL2QL","dataAllValuesFrom"), args) =>
	  	      val property = dataPropertyToOWL(args(0))
	  	      val filler = dataRangeToOWL(args(1))
	  	      dataFactory.getOWLDataAllValuesFrom(property, filler)
	  	        
	  	 case OMA(OWL2OMS("OWL2EL","dataSomeValuesFrom"), args) =>
	  	      val property = dataPropertyToOWL(args(0))
	  	      val filler = dataRangeToOWL(args(1))
	  	      dataFactory.getOWLDataSomeValuesFrom(property, filler)

	  	 case OMA(OWL2OMS("OWL2SUB","objectExactCardinality"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = propertyToOWL(args(1))
	  	      dataFactory.getOWLObjectExactCardinality(cardinality, property)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","objectMinCardinality"), args) =>
	  	      val cardinality = termToInt(args(0)) 
	  	      val property = propertyToOWL(args(1))
	  	      dataFactory.getOWLObjectMinCardinality(cardinality, property)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","objectMaxCardinality"), args) =>  
	  	      val cardinality = termToInt(args(0))
	  	      val property = propertyToOWL(args(1))
	  	      dataFactory.getOWLObjectMaxCardinality(cardinality, property)
	  	 
	  	 case OMA(OWL2OMS("OWL2SUB","objectExactCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = propertyToOWL(args(1))
	  	      val filler = classToOWL(args(2))
	  	      dataFactory.getOWLObjectExactCardinality(cardinality, property, filler)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","objectMinCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = propertyToOWL(args(1))
	  	      val filler = classToOWL(args(2))
	  	      dataFactory.getOWLObjectMinCardinality(cardinality, property, filler)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","objectMaxCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = propertyToOWL(args(1))
	  	      val filler = classToOWL(args(2))
	  	      dataFactory.getOWLObjectMinCardinality(cardinality, property, filler)
	  	     
	  	 case OMA(OWL2OMS("OWL2SUB","dataExactCardinality"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      dataFactory.getOWLDataExactCardinality(cardinality, property)
	  	       
	  	 case OMA(OWL2OMS("OWL2SUB","dataMinCardinality"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      dataFactory.getOWLDataMinCardinality(cardinality, property)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","dataMaxCardinality"), args) => 
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      dataFactory.getOWLDataMaxCardinality(cardinality, property)
	  	 
	  	 case OMA(OWL2OMS("OWL2SUB","dataExactCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      val dataRange = dataRangeToOWL(args(2))
	  	      dataFactory.getOWLDataExactCardinality(cardinality, property, dataRange)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","dataMinCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      val dataRange = dataRangeToOWL(args(2))
	  	      dataFactory.getOWLDataMinCardinality(cardinality, property, dataRange)
	  	      
	  	 case OMA(OWL2OMS("OWL2SUB","dataMaxCardinalityQualified"), args) =>
	  	      val cardinality = termToInt(args(0))
	  	      val property = dataPropertyToOWL(args(1))
	  	      val dataRange = dataRangeToOWL(args(2))
	  	      dataFactory.getOWLDataMaxCardinality(cardinality, property, dataRange)
	  	
  	 	 case OMA(OWL2OMS("OWL2SUB", "objectOneOf"), args) =>
	  	  	  val argsList = args.map(individualToOWL)
	  	      dataFactory.getOWLObjectOneOf(argsList.toSet)
	  	      
	     case OMA(OWL2OMS("OWL2SUB", "objectHasValue"), args) =>
	     	  val property = propertyToOWL(args(0))
	     	  val value = individualToOWL(args(1))
	  	 	  dataFactory.getOWLObjectHasValue(property, value)
	  	 
	  	 case OMA(OWL2OMS("OWL2SUB", "objectHasSelf"), args) =>
	  	 	  val property = propertyToOWL(args(0))
	  	 	  dataFactory.getOWLObjectHasSelf(property)
	  	 	
         case c => throw Exception("unsupported class: " + c)       
	   }
   }

   def individualToOWL(t: Term) : OWLIndividual = {
	   t match {
	  	 case OMID(p) => dataFactory.getOWLNamedIndividual(globalNameToIRI(p))
	   //case getOWLAnonymousIndividual getIDI
	  	 case _ => throw Exception("none of the individuals")
	   }
   }
  
   def propertyToOWL(t: Term) : OWLObjectPropertyExpression = {
   		 t match  {
	  	   case OMID(p) => dataFactory.getOWLObjectProperty(globalNameToIRI(p))
	  	   case OMA(OWL2OMS("OWL2QL", "objectInverseOf"), args) =>
	  	        val inverse = propertyToOWL(args(0))
	  	        dataFactory.getOWLObjectInverseOf(inverse)
	  	   case _ => throw Exception("none of the object property expressions") 
	  	}
   }
   
   def dataPropertyToOWL(t: Term) : OWLDataPropertyExpression = {
	   t match {
	     case OMID(p) => dataFactory.getOWLDataProperty(globalNameToIRI(p))
	     case _ => throw Exception("none of the data property expressions")
	   }
   }
  
   def dataRangeToOWL(t : Term) : OWLDataRange = {
   	   t match {
		 case OMID(p) =>
   		     val iri : IRI = t match {
   		    	 case OWLOMS("D1",d1) => IRI.create("http://www.w3.org/2001/XMLSchema#" + d1)
   		    	  		    	
   		    	 case OWL2OMS("D2","real") => IRI.create("http://www.w3.org/2002/07/owl#real")
   		    	 case OWL2OMS("D2","rational") => IRI.create("http://www.w3.org/2002/07/owl#rational")
   		    	 case OWL2OMS("D2","dateTimeStamp") => IRI.create("http://www.w3.org/2001/XMLSchema#dateTimeStamp" )
   		    	 case OWL2OMS("D2","xmlLiteral") => IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral" )
   		    	 case OWL2OMS("D2","plainLiteral") => IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral" )
   		    	 //PlainLiteral ?
   		    	 case _ => println("other data types")
   		    	 		   globalNameToIRI(p)
   		      }
   		     dataFactory.getOWLDatatype(iri)
   		      		   
   		   case OMA(OWL2OMS("OWL2SUB", "dataIntersectionOf"), args) =>
	  	        val argsList = args.map(dataRangeToOWL)
	  	        dataFactory.getOWLDataIntersectionOf(argsList.toSet)
	  	        
	  	   case OMA(OWL2OMS("OWL2QL", "dataUnionOf"), args) =>     
	  	        val argsList = args.map(dataRangeToOWL)
	  	        dataFactory.getOWLDataUnionOf(argsList.toSet)
	  	        
	  	   case OMA(OWL2OMS("OWL2QL", "dataComplementOf"), args) =>
	  	        val dataRange = dataRangeToOWL(args(0))
	  	        dataFactory.getOWLDataComplementOf(dataRange)
 	  	  	        	  	        
	  	  case OMA(OWL2OMS("OWL2SUB","dataOneOf"), args) =>
	  	        val argsList = args.map(literalToOWL)
	  	        dataFactory.getOWLDataOneOf(argsList.toSet)
	 	        
	  	  case OMA(OWL2OMS("OWL2SUB","dataTypeRestriction"), args) =>
	  	        val dataType = dataRangeToOWL(args(0)) match {
	  	        									   case d : OWLDatatype => d	
	  	        									   case _ => throw Exception("not a data type")
	  	        			   }
	  	        val argsList = args.tail.map(facetToOWL)
	  	        // argsList.toSet returns one-element set even when argsList has two different elements. Bug? 
	  	        dataFactory.getOWLDatatypeRestriction(dataType, asJavaSet(argsList.toSet))
	  	    	  	    
          case _ => throw Exception("none of the data ranges")
   	   }
   }

   def literalToOWL(t : Term) : OWLLiteral = {
	    t match {
		case OMF(lt) => dataFactory.getOWLLiteral(lt)
		case OMI(lt) => dataFactory.getOWLLiteral(lt.toInt)
		case OMSTR(lt) => dataFactory.getOWLLiteral(lt)
		//string and others
		case OMA(OWL2OMS("OWL2SUB","literal"),args) =>
		val lexicalValue = args(0) match {
			case OMSTR(s) => s
			case _ => throw Exception("not a string")
		}
		val dataType = dataRangeToOWL(args(1)) match {
	  	        							   case d : OWLDatatype => d	
	  	        							   case _ => throw Exception("not a data type")
		}  
		dataFactory.getOWLLiteral(lexicalValue, dataType)
 	    case _ => throw Exception("none of the literals")
	    }
	}

   def facetToOWL(t : Term) : OWLFacetRestriction = {
	   t match {
	  	 case OMA(OWL2OMS("OWL2SUB", "facetRestriction"), args) =>
	  	 val facet = args(0) match {
	  		 case OWL2OMS("OWL2SUB", f) =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#" + f))
	  	/*	 case OWL2OMS("OWL2SUB", "minInclusive") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#minInclusive"))
	  		 case OWL2OMS("OWL2SUB", "maxInclusive") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#maxInclusive"))
	  		 case OWL2OMS("OWL2SUB", "minExclusive") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#minExclusive"))
	  		 case OWL2OMS("OWL2SUB", "maxExclusive") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#maxExclusive"))
	  		 case OWL2OMS("OWL2SUB", "maxLength") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#maxLength"))
	  		 case OWL2OMS("OWL2SUB", "minLength") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#minLength"))
	  		 
        */	  		 
	  	 }
	     val facetValue = literalToOWL(args(1))
	   	 dataFactory.getOWLFacetRestriction(facet, facetValue) 
	   	 // case other facets
	  	 case _ => throw Exception("not a facet restriction")
	   }
   }
 
   def termToInt (t: Term) : Int = {
	   t match { 
	  	 case OMI(i) => i.toInt 
	  	 case _ => throw Exception("not an integer")
	   }
   }
   
   def constantToOWL(constant : Constant) {
	   val consName : LocalName = constant.name  
	   println(consName)
	   val consType : Term = (constant.tp match {
	   	   case Some(t) => t
	   	   case None => throw Exception("")
	   })
	  val path = constant.path
	  	  
	  val axiom = consType match {
// DeclarationAxiom	  	  
	  	  case OWLOMS("OWLBase", "class") =>
	  	      val clss = dataFactory.getOWLClass(globalNameToIRI(path))
	  	      dataFactory.getOWLDeclarationAxiom(clss) 			  	   		
	  	      	  	       
	  	  case OWLOMS("OWLBase", "individual") =>
	  	   	  val individual = dataFactory.getOWLNamedIndividual(globalNameToIRI(path)) 
	  	   	  dataFactory.getOWLDeclarationAxiom(individual) 			  	   		
	  	   	  	  	   	  
	      case OWLOMS("OWLBase", "objectProperty") => 	
	  	   	  val objectProperty = dataFactory.getOWLObjectProperty(globalNameToIRI(path)) 
	  	   	  dataFactory.getOWLDeclarationAxiom(objectProperty)
	  	   	  	  	   	   
	  	  case OWL2OMS("OWL2SUB", "dataProperty") => 		
	  	   	  val dataProperty = dataFactory.getOWLDataProperty(globalNameToIRI(path)) 
	  	   	  dataFactory.getOWLDeclarationAxiom(dataProperty)
	  	   	  	  	   	  
	  	  case OWLOMS("D1", "dataType") =>
	  	   	  val dataType = dataFactory.getOWLDatatype(globalNameToIRI(path))
	  	   	  dataFactory.getOWLDeclarationAxiom(dataType)
// ClassAxiom	     
	  	  case OMA(OWL2OMS("OWL2SUB", "subClassOf"), args) =>
	  	   	   val subClass = classToOWL(args(0))
	  	       val superClass = classToOWL(args(1))
	  	       dataFactory.getOWLSubClassOfAxiom(subClass, superClass) 
	  	       	  	        
	  	  case OMA(OWL2OMS("OWL2SUB", "disjointUnionOf"), args) =>
	  	       val firstClass = classToOWL(args(0)) match {
	  	      					case c : OWLClass => c
	  	      					case _ => throw Exception("not a class")
	  	       }
	  	       val argsList = args.tail.map(classToOWL)
	  	       dataFactory.getOWLDisjointUnionAxiom(firstClass, asJavaSet(argsList.toSet))
	  	       	  	   	       
	  	  case OMA(OWL2OMS("OWL2SUB", "equivalentClasses"), args) =>
	  	      	val argsList = args.map(classToOWL)
	  	      	dataFactory.getOWLEquivalentClassesAxiom(argsList.toSet)
	  	      	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "disjointClasses"), args) =>
	  	      	val argsList = args.map(classToOWL)
	  	      	dataFactory.getOWLDisjointClassesAxiom(argsList.toSet)
// ObjectPropertyAxiom
    	  case OMA(OWL2OMS("OWL2SUB", "subObjectPropertyOf"),args) =>
	  	      	val subProperty = propertyToOWL(args(0))
	  	      	val superProperty = propertyToOWL(args(1))
	  	      	dataFactory.getOWLSubObjectPropertyOfAxiom(subProperty, superProperty)
	  	      		  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "equivalentObjectProperty"),args) =>
	  	      	val argsList = args.map(propertyToOWL)
	  	      	dataFactory.getOWLEquivalentObjectPropertiesAxiom(argsList.toSet)
	  	      		  	  	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "disjointObjectProperty"),args) =>
	  	      	val argsList = args.map(propertyToOWL)
	  	      	dataFactory.getOWLDisjointObjectPropertiesAxiom(argsList.toSet)
  	      		  	      		
  	      case OMA(OWL2OMS("OWL2QL", "inverseObjectProperties"),args) =>
	  	      	val forwardProperty = propertyToOWL(args(0))
	  	      	val inverseProperty = propertyToOWL(args(1))
	  	      	dataFactory.getOWLInverseObjectPropertiesAxiom(forwardProperty, inverseProperty)
	  	      		  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "objectPropertyDomain"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val domain = classToOWL(args(1))
	  	      	dataFactory.getOWLObjectPropertyDomainAxiom(property, domain)
	  	        	  	        
	  	   case OMA(OWL2OMS("OWL2SUB", "objectPropertyRange"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val range = classToOWL(args(1))
	  	      	dataFactory.getOWLObjectPropertyRangeAxiom(property, range)
	  	        
	  	   case OMA(OWL2OMS("OWL2SUB","functionalObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	dataFactory.getOWLFunctionalObjectPropertyAxiom(property)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2QL","inverseFunctionalObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	dataFactory.getOWLInverseFunctionalObjectPropertyAxiom(property)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","reflexiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	        dataFactory.getOWLReflexiveObjectPropertyAxiom(property)
	  	      		  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","irreflexiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	dataFactory.getOWLIrreflexiveObjectPropertyAxiom(property)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","symmetricObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	dataFactory.getOWLSymmetricObjectPropertyAxiom(property)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","asymmetricObjetProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	        dataFactory.getOWLAsymmetricObjectPropertyAxiom(property)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","transitiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	dataFactory.getOWLTransitiveObjectPropertyAxiom(property)
// DataPropertyAxiom	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "subDataPropertyOf"),args) =>
	  	      	val subProperty = dataPropertyToOWL(args(0))
	  	      	val superProperty = dataPropertyToOWL(args(1))
	  	      	dataFactory.getOWLSubDataPropertyOfAxiom(subProperty, superProperty)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "equivalentDataProperties"),args) =>
	  	      	val argsList = args.map(dataPropertyToOWL)
	  	      	dataFactory.getOWLEquivalentDataPropertiesAxiom(argsList.toSet)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "disjointDataProperties"),args) =>
	  	      	val argsList = args.map(dataPropertyToOWL)
	  	      	dataFactory.getOWLDisjointDataPropertiesAxiom(argsList.toSet)
  	      		  	      		
  	       case OMA(OWL2OMS("OWL2SUB", "dataPropertyDomain"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	val domain = classToOWL(args(1))
	  	      	dataFactory.getOWLDataPropertyDomainAxiom(property, domain)
	  	        	  	        
	  	   case OMA(OWL2OMS("OWL2SUB", "dataPropertyRange"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	val range = dataRangeToOWL(args(1))
	  	      	dataFactory.getOWLDataPropertyRangeAxiom(property, range)
	  	        	  	        
	  	   case OMA(OWL2OMS("OWL2SUB","functionalDataProperty"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	dataFactory.getOWLFunctionalDataPropertyAxiom(property)
// DatatypeDefinitionAxiom	  	     	
  	      case OMA(OWL2OMS("OWL2SUB", "dataTypeDefinition"), args) =>
	  	     	val dataType = dataRangeToOWL(args(0)) match {
	  	     		case d : OWLDatatype => d
	  	     		case _ => throw Exception("not a data type")
	  	     	}
	  	     	val dataRange = dataRangeToOWL(args(1))
	  	     	dataFactory.getOWLDatatypeDefinitionAxiom(dataType, dataRange)	
// HasKeyAxiom	    	     	
// AssertionAxiom
	  	   case OMA(OWL2OMS("OWL2SUB", "classAssertion"), args) =>
	  	     	val clss = classToOWL(args(0))
	  	     	val individual = individualToOWL(args(1))
	  	     	dataFactory.getOWLClassAssertionAxiom(clss, individual)
	  	     		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","sameIndividual"), args) =>
	  	     	val argsList = args.map(individualToOWL)
	  	      	dataFactory.getOWLSameIndividualAxiom(argsList.toSet)
  	      		  	      
  	      case OMA(OWL2OMS("OWL2SUB","differentIndividuals"), args) =>
	  	     	val argsList = args.map(individualToOWL)
	  	      	dataFactory.getOWLDifferentIndividualsAxiom(argsList.toSet)
  	      		  	      		
  	      case OMA(OWL2OMS("OWL2SUB", "objectPropertyAssertion"), args) =>
	  	     	val property = propertyToOWL(args(0))
	  	     	val individual = individualToOWL(args(1))
	  	     	val obj = individualToOWL(args(2))
	  	     	dataFactory.getOWLObjectPropertyAssertionAxiom(property,individual,obj)
	  	     		  	     	
  	      case OMA(OWL2OMS("OWL2SUB", "negativeObjectPropertyAssertion"), args) =>
	  	     	val property = propertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = individualToOWL(args(2))
	  	     	dataFactory.getOWLNegativeObjectPropertyAssertionAxiom(property,subj,obj)
	  	     	
  	      case OMA(OWL2OMS("OWL2SUB", "dataPropertyAssertion"), args) =>
	  	     	val property = dataPropertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = literalToOWL(args(2)) //   float value icin ayri method var
	  	     	dataFactory.getOWLDataPropertyAssertionAxiom(property,subj,obj)
 	      		
  	      case OMA(OWL2OMS("OWL2SUB", "negativeDataPropertyAssertion"), args) =>
	  	     	val property = dataPropertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = literalToOWL(args(2))
	  	     	dataFactory.getOWLNegativeDataPropertyAssertionAxiom(property,subj,obj)
// AnnotationAxiom 
	  	  //case OMA(OWL2OMS(),args) =>
	  	     
     	  case _ => throw Exception("none of the classAxioms")       
	     } 
	      manager.addAxiom(ontology, axiom)
        //case consType : Option[OMSTR]
	  	//case consType : Option[OMI]
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
		val source : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Literal\\literal.omdoc")		
		val target : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Literal\\literalToOWL.owl") 
		
		val doc : DPath  = controller.read(source)
		
		def writeToFile(iri : IRI, trg : File) {
			val onto = manager.getOntology(iri)
			val file = new java.io.FileWriter(trg)
			val ontoTarget = new WriterDocumentTarget(file)
			val OWLXMLformat = new OWLXMLOntologyFormat()
			manager.saveOntology(onto, OWLXMLformat, ontoTarget)
			file.close
		}
		
		val iris : List[IRI] = try {
			exporter.documentToOWL(doc)
		} catch {
			case e @ Exception(msg) =>
			  println(msg)
			  throw e
		}
		if (iris.length == 1) {
			writeToFile(iris.head, target)
		} else {
			target.mkdirs()
		    iris.foreach {iri => writeToFile(iri, new File(target, Utils.IRILast(iri)))}
		}
	}
}
