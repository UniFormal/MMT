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
	  	 
         case OMA(OWL2OMS("OWL2QL","objectSomeValuesFrom"),args) =>   
	  	      val property = propertyToOWL(args(0))
	  	      val filler = classToOWL(args(1))
	  	      dataFactory.getOWLObjectSomeValuesFrom(property, filler)
  	        
         case OMA(OWL2OMS("OWL2QL","dataAllValuesFrom"), args) =>
	  	      val property = dataPropertyToOWL(args(0))
	  	      val filler = dataRangeToOWL(args(1))
	  	      dataFactory.getOWLDataAllValuesFrom(property, filler)
	  	        
	  	 case OMA(OWL2OMS("OWL2QL","dataSomeValuesFrom"), args) =>
	  	      val property = dataPropertyToOWL(args(0))
	  	      val filler = dataRangeToOWL(args(1))
	  	      dataFactory.getOWLDataSomeValuesFrom(property, filler)
/*
	  	 case OMA(OWL2OMS("OWL2SUB","objectExactCardinality"), args) =>
	  	 case OMA(OWL2OMS("OWL2SUB","objectMinCardinality"), args) =>
	  	 case OMA(OWL2OMS("OWL2SUB","objectMaxCardinality"), args) =>      
	  	 // isQualified
	  	 
	  	 case OMA(OWL2OMS("OWL2SUB","dataExactCardinality"), args) =>
	  	 case OMA(OWL2OMS("OWL2SUB","dataMinCardinality"), args) =>
	  	 case OMA(OWL2OMS("OWL2SUB","dataMaxCardinality"), args) =>      
	  	 // isQualified
*/	  	 
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
	  	 	
         case _ => throw Exception("none of the terms")       
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
   		     val s = p match {
   		    	 case OWLOMS("D1","boolean") => "xsd:boolean"
   		    	 case OWLOMS("D1","double") => "xsd:double"
   		    	 case OWLOMS("D1","float") => "xsd:float"
   		    	 case OWLOMS("D1","integer") => "xsd:integer"
   		    	 case OWL2OMS("D2","PlainLiteral") => "rdf:PlainLiteral"
   		    	 case OWLOMS("D1","string") => "xsd:string"
   		    	 case _ => throw Exception("none of the data types")
   		      }
   		      val iri : IRI = IRI.create(s)
   		      dataFactory.getOWLDatatype(iri)
   		      		   
   		   case OMA(OWL2OMS("OWL2SUB", "dataIntersectionOf"), args) =>
	  	        val argsList = args.map(dataRangeToOWL)
	  	        dataFactory.getOWLDataIntersectionOf(argsList.toSet)
	  	        
	  	   case OMA(OWL2OMS("OWL2SUB", "dataUnionOf"), args) =>     
	  	        val argsList = args.map(dataRangeToOWL)
	  	        dataFactory.getOWLDataUnionOf(argsList.toSet)
	  	        
	  	   case OMA(OWL2OMS("OWL2QL", "dataComplementOf"), args) =>
	  	        val dataRange = dataRangeToOWL(args(0))
	  	        dataFactory.getOWLDataComplementOf(dataRange)
 /* 	  	  	        	  	        
	  	  case OMA(OWL2OMS("OWL2SUB","dataOneOf"), args) =>
	  	        val argsList = args.map(literalToOWL)
	  	        dataFactory.getOWLDataComplementOf(argsList.toSet)
	  	        
	  	  case OMA(OWL2OMS("OWL2SUB","dataTypeRestriction"), args) =>
	  	        val dataType = dataRangeToOWL(args(0))
	  	        val argsList =  args.tail.map(facetToOWL)
	  	        dataFactory.getOWLDatatypeRestriction(dataType :: asJavaSet(argsList.toSet))
*/	  	   
          case _ => throw Exception("none of the data ranges")
   	   }
   }

   def literalToOWL(t : Term) : OWLLiteral = {
	    t match {
		case OMF(lt) => dataFactory.getOWLLiteral(lt)
		//case OMI(lt) => dataFactory.getOWLLiteral(lt)
 	    case _ => throw Exception("none of the literals")
	    }
	}

   def facetToOWL(t : Term) : OWLFacetRestriction = {
	   null
   }
   
   def constantToOWL(constant : Constant) {
	   val consName : LocalName = constant.name  
	   println(consName)
	   val consType : Term = (constant.tp match {
	   	   case Some(t) => t
	   	   case None => throw Exception("")
	   })
	  val path = constant.path
	  	  
	 /* val entity = consType match {
	  	  case OWLOMS("OWLBase", "class") =>
	  	   	  dataFactory.getOWLClass(globalNameToIRI(path))
	  	  case OWLOMS("OWLBase", "individual") =>
	  	   	  dataFactory.getOWLNamedIndividual(globalNameToIRI(path)) 
	      case OWLOMS("OWLBase", "objectProperty") => 	
	  	   	  dataFactory.getOWLObjectProperty(globalNameToIRI(path)) 
	  	  case OWL2OMS("OWL2SUB", "dataProperty") => 		
	  	   	  dataFactory.getOWLDataProperty(globalNameToIRI(path)) 
	  	  case OWLOMS("D1", "dataType") =>
	  	   	  dataFactory.getOWLDatatype(globalNameToIRI(path))
	   }
	   val declarationAxiom = dataFactory.getOWLDeclarationAxiom(entity) 			  	   		
	   manager.addAxiom(ontology, declarationAxiom)
 	  */ 
	   consType match {
	  	   
// DeclarationAxiom	  	  
	  	  case OWLOMS("OWLBase", "class") =>
	  	      val clss = dataFactory.getOWLClass(globalNameToIRI(path))
	  	      val declarationAxiom = dataFactory.getOWLDeclarationAxiom(clss) 			  	   		
	  	      manager.addAxiom(ontology, declarationAxiom)
	  	       
	  	  case OWLOMS("OWLBase", "individual") =>
	  	   	  val individual = dataFactory.getOWLNamedIndividual(globalNameToIRI(path)) 
	  	   	  val declarationAxiom = dataFactory.getOWLDeclarationAxiom(individual) 			  	   		
	  	   	  manager.addAxiom(ontology, declarationAxiom)
	  	   	  
	      case OWLOMS("OWLBase", "objectProperty") => 	
	  	   	  val objectProperty = dataFactory.getOWLObjectProperty(globalNameToIRI(path)) 
	  	   	  val declarationAxiom = dataFactory.getOWLDeclarationAxiom(objectProperty)
	  	   	  manager.addAxiom(ontology, declarationAxiom)
	  	   	   
	  	  case OWL2OMS("OWL2SUB", "dataProperty") => 		
	  	   	  val dataProperty = dataFactory.getOWLDataProperty(globalNameToIRI(path)) 
	  	   	  val declarationAxiom = dataFactory.getOWLDeclarationAxiom(dataProperty)
	  	   	  manager.addAxiom(ontology, declarationAxiom)
	  	   	  
	  	  case OWLOMS("D1", "dataType") =>
	  	   	  val dataType = dataFactory.getOWLDatatype(globalNameToIRI(path))
	  	   	  val declarationAxiom = dataFactory.getOWLDeclarationAxiom(dataType)
	  	   	  manager.addAxiom(ontology, declarationAxiom)
// ClassAxiom	     
 	   
	   // consType match {
	      case OMA(OWL2OMS("OWL2SUB", "subClassOf"), args) =>
	  	      	val subClass = classToOWL(args(0))
	  	      	val superClass = classToOWL(args(1))
	  	        val subClassOfAxiom = dataFactory.getOWLSubClassOfAxiom(subClass, superClass) 
	  	        manager.addAxiom(ontology, subClassOfAxiom)	
	  	   
	  	  case OMA(OWL2OMS("OWL2SUB", "equivalentClasses"), args) =>
	  	      	val argsList = args.map(classToOWL)
	  	      	val equivalentClassesAxiom = dataFactory.getOWLEquivalentClassesAxiom(argsList.toSet)
	  	      	manager.addAxiom(ontology, equivalentClassesAxiom)
	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "disjointClasses"), args) =>
	  	      	val argsList = args.map(classToOWL)
	  	      	val disjointClassesAxiom = dataFactory.getOWLDisjointClassesAxiom(argsList.toSet)
	  	      	manager.addAxiom(ontology, disjointClassesAxiom)
	  	      	
     	  case OMA(OWL2OMS("OWL2SUB", "disjointUnionOf"), args) =>
	  	      	val firstClass = classToOWL(args(0)) match {
	  	      			case c : OWLClass => c
	  	      			case _ => throw Exception("not a class")
	  	      	}
	  	      	val argsList = args.tail.map(classToOWL)
	  	      	val disjointUnionOfAxiom = dataFactory.getOWLDisjointUnionAxiom(firstClass, asJavaSet(argsList.toSet))
	  	      	manager.addAxiom(ontology, disjointUnionOfAxiom)

// ObjectPropertyAxiom
    	  case OMA(OWL2OMS("OWL2SUB", "subObjectPropertyOf"),args) =>
	  	      	val subProperty = propertyToOWL(args(0))
	  	      	val superProperty = propertyToOWL(args(1))
	  	      	val subObjectPropertyOfAxiom = dataFactory.getOWLSubObjectPropertyOfAxiom(subProperty, superProperty)
	  	      	manager.addAxiom(ontology, subObjectPropertyOfAxiom)
	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "equivalentObjectProperty"),args) =>
	  	      	val argsList = args.map(propertyToOWL)
	  	      	val equivalentObjectPropertyAxiom = dataFactory.getOWLEquivalentObjectPropertiesAxiom(argsList.toSet)
	  	      	manager.addAxiom(ontology, equivalentObjectPropertyAxiom)
	  	  	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "disjointObjectProperty"),args) =>
	  	      	val argsList = args.map(propertyToOWL)
	  	      	val disjointObjectPropertyAxiom = dataFactory.getOWLDisjointObjectPropertiesAxiom(argsList.toSet)
  	      		manager.addAxiom(ontology, disjointObjectPropertyAxiom)
  	      		
  	      case OMA(OWL2OMS("OWL2QL", "inverseObjectProperties"),args) =>
	  	      	val forwardProperty = propertyToOWL(args(0))
	  	      	val inverseProperty = propertyToOWL(args(1))
	  	      	val inverseObjectPropertiesAxiom = dataFactory.getOWLInverseObjectPropertiesAxiom(forwardProperty, inverseProperty)
	  	      	manager.addAxiom(ontology, inverseObjectPropertiesAxiom)
	  	      	
	  	  case OMA(OWL2OMS("OWL2SUB", "objectPropertyDomain"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val domain = classToOWL(args(1))
	  	      	val objectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(property, domain)
	  	        manager.addAxiom(ontology, objectPropertyDomainAxiom)
	  	        
	  	   case OMA(OWL2OMS("OWL2SUB", "objectPropertyRange"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val range = classToOWL(args(1))
	  	      	val objectPropertyRangeAxiom = dataFactory.getOWLObjectPropertyRangeAxiom(property, range)
	  	        manager.addAxiom(ontology, objectPropertyRangeAxiom)
	 
	  	   case OMA(OWL2OMS("OWL2SUB","functionalObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val functionalObjectPropertyAxiom = dataFactory.getOWLFunctionalObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, functionalObjectPropertyAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2QL","inverseFunctionalObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val inverseFunctionalObjectPropertyAxiom = dataFactory.getOWLInverseFunctionalObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, inverseFunctionalObjectPropertyAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","reflexiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val reflexiveObjectPropertyAxiom = dataFactory.getOWLReflexiveObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, reflexiveObjectPropertyAxiom)
	  	      		  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","irreflexiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val irreflexiveObjectPropertyAxiom = dataFactory.getOWLIrreflexiveObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, irreflexiveObjectPropertyAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","symmetricObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val symmetricObjectPropertyAxiom = dataFactory.getOWLSymmetricObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, symmetricObjectPropertyAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","asymmetricObjetProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	        val asymmetricObjectPropertyAxiom = dataFactory.getOWLAsymmetricObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, asymmetricObjectPropertyAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","transitiveObjectProperty"),args) =>
	  	      	val property = propertyToOWL(args(0))
	  	      	val transitiveObjectPropertyAxiom = dataFactory.getOWLTransitiveObjectPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, transitiveObjectPropertyAxiom)
// DataPropertyAxiom	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "subDataPropertyOf"),args) =>
	  	      	val subProperty = dataPropertyToOWL(args(0))
	  	      	val superProperty = dataPropertyToOWL(args(1))
	  	      	val subDataPropertyOfAxiom = dataFactory.getOWLSubDataPropertyOfAxiom(subProperty, superProperty)
	  	      	manager.addAxiom(ontology, subDataPropertyOfAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "equivalentDataProperty"),args) =>
	  	      	val argsList = args.map(dataPropertyToOWL)
	  	      	val equivalentDataPropertyAxiom = dataFactory.getOWLEquivalentDataPropertiesAxiom(argsList.toSet)
	  	      	manager.addAxiom(ontology, equivalentDataPropertyAxiom)	  	      	
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB", "disjointDataProperties"),args) =>
	  	      	val argsList = args.map(dataPropertyToOWL)
	  	      	val disjointDataPropertiesAxiom = dataFactory.getOWLDisjointDataPropertiesAxiom(argsList.toSet)
  	      		manager.addAxiom(ontology, disjointDataPropertiesAxiom)	 
  	      		
  	       case OMA(OWL2OMS("OWL2SUB", "dataPropertyDomain"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	val domain = classToOWL(args(1))
	  	      	val dataPropertyDomainAxiom = dataFactory.getOWLDataPropertyDomainAxiom(property, domain)
	  	        manager.addAxiom(ontology, dataPropertyDomainAxiom)
	  	        
	  	   case OMA(OWL2OMS("OWL2SUB", "dataPropertyRange"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	val range = dataRangeToOWL(args(1))
	  	      	val dataPropertyRangeAxiom = dataFactory.getOWLDataPropertyRangeAxiom(property, range)
	  	        manager.addAxiom(ontology, dataPropertyRangeAxiom)
	  	        
	  	   case OMA(OWL2OMS("OWL2SUB","functionalDataProperty"),args) =>
	  	      	val property = dataPropertyToOWL(args(0))
	  	      	val functionalDataPropertyAxiom = dataFactory.getOWLFunctionalDataPropertyAxiom(property)
	  	      	manager.addAxiom(ontology, functionalDataPropertyAxiom)
	  	         	      		
// AssertionAxiom
	  	   case OMA(OWL2OMS("OWL2SUB", "classAssertion"), args) =>
	  	     	val clss = classToOWL(args(0))
	  	     	val individual = individualToOWL(args(1))
	  	     	val classAssertionAxiom = dataFactory.getOWLClassAssertionAxiom(clss, individual)
	  	     	manager.addAxiom(ontology, classAssertionAxiom)
	  	      	
	  	   case OMA(OWL2OMS("OWL2SUB","sameIndividual"), args) =>
	  	     	val argsList = args.map(individualToOWL)
	  	      	val sameIndividualAxiom = dataFactory.getOWLSameIndividualAxiom(argsList.toSet)
  	      		manager.addAxiom(ontology, sameIndividualAxiom)
  	      
  	      case OMA(OWL2OMS("OWL2SUB","differentIndividuals"), args) =>
	  	     	val argsList = args.map(individualToOWL)
	  	      	val differentIndividualsAxiom = dataFactory.getOWLDifferentIndividualsAxiom(argsList.toSet)
  	      		manager.addAxiom(ontology, differentIndividualsAxiom)
  	      		
  	      case OMA(OWL2OMS("OWL2SUB", "objectPropertyAssertion"), args) =>
	  	     	val property = propertyToOWL(args(0))
	  	     	val individual = individualToOWL(args(1))
	  	     	val obj = individualToOWL(args(2))
	  	     	val objectPropertyAssertionAxiom = dataFactory.getOWLObjectPropertyAssertionAxiom(property,individual,obj)
	  	     	manager.addAxiom(ontology, objectPropertyAssertionAxiom)
	  	     	
  	      case OMA(OWL2OMS("OWL2SUB", "negativeObjectPropertyAssertion"), args) =>
	  	     	val property = propertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = individualToOWL(args(2))
	  	     	val negativeObjectPropertyAssertionAxiom = dataFactory.getOWLNegativeObjectPropertyAssertionAxiom(property,subj,obj)
	  	     	manager.addAxiom(ontology, negativeObjectPropertyAssertionAxiom)	  	     	
	  	     	
  	      case OMA(OWL2OMS("OWL2SUB", "dataPropertyAssertion"), args) =>
	  	     	val property = dataPropertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = literalToOWL(args(2)) //   float value icin ayri method var
	  	     	val dataPropertyAssertionAxiom = dataFactory.getOWLDataPropertyAssertionAxiom(property,subj,obj)
	  	     	manager.addAxiom(ontology, dataPropertyAssertionAxiom)	  	     	
  	      		
  	      case OMA(OWL2OMS("OWL2SUB", "negativeDataPropertyAssertion"), args) =>
	  	     	val property = dataPropertyToOWL(args(0))
	  	     	val subj = individualToOWL(args(1))
	  	     	val obj = literalToOWL(args(2))
	  	     	val negativeDataPropertyAssertionAxiom = dataFactory.getOWLNegativeDataPropertyAssertionAxiom(property,subj,obj)
	  	     	manager.addAxiom(ontology, negativeDataPropertyAssertionAxiom)		
  	  
	  	     	
	  	     	
/*

// DatatypeDefinitionAxiom
// HasKeyAxiom
// AnnotationAxiom 
 
  	  	  case OMA(OWL2OMS(),args) =>
*/	  	     
     	  case _ => throw Exception("none of the classAxioms")       
	     }
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
		val source : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Base\\Base.omdoc")		
		val target : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Base\\BaseToOWL.owl") 
		
		val doc : DPath  = controller.read(source)
		
		def writeToFile(iri : IRI, trg : File) {
			val onto = manager.getOntology(iri)
			val file = new java.io.FileWriter(trg)
			val ontoTarget = new WriterDocumentTarget(file)
			val OWLXMLformat = new OWLXMLOntologyFormat()
			manager.saveOntology(onto, OWLXMLformat, ontoTarget)
			file.close
		}
		
		val iris : List[IRI] = exporter.documentToOWL(doc)
		if (iris.length == 1) {
			writeToFile(iris.head, target)
		} else {
			target.mkdirs()
		    iris.foreach {iri => writeToFile(iri, new File(target, Utils.IRILast(iri)))}
		}
	}
}
