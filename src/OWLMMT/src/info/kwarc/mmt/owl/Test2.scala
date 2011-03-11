package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.File
import java.net.URI

import scala.collection.mutable.Set // 
import scala.collection.JavaConversions._ //
import jomdoc._
import jomdoc.symbols.Constant
import jomdoc.frontend._
import jomdoc.modules._
import jomdoc.libraries._
import jomdoc.documents._
import jomdoc.lf._
import jomdoc.objects.{Term,OMS}

/*
import java.io.FileNotFoundException
import java.io.IOException
import java.net.UnknownHostException
import java.util.Map 
*/

object Test2 {
	var num : Int = 1
	var currentThy : MPath = null 
	def printClass(c : OWLClassExpression ){println(c)}
	def printIndividual(i : OWLIndividual){println(i)}
	def printProperty(p : OWLObjectPropertyExpression){println(p)}
	def printObject(o: OWLPropertyAssertionObject){println(o)}
	
	def classToLF(c : OWLClassExpression) : Term = {   
		c match {
			
			case c : OWLClass =>
		        CurrOMS(c.getIRI)
		        
		    case c : OWLObjectIntersectionOf =>
		        val name = new LocalPath("ax" + num)
		        num = num + 1
		        val args = c.getOperandsAsList.map(classToLF)
		        ApplySpine(OWLOMS("OWL2SUB", "objectIntersectionOf"), args : _*)
		             		         
		    case c : OWLObjectUnionOf =>  
		        val name = new LocalPath("ax" + num)
		        num = num + 1
		        val args = c.getOperandsAsList.map(classToLF)
		        ApplySpine(OWLOMS("OWL2QL", "objectUnionOf"), args : _*)
		    		     
		    case c : OWLObjectComplementOf =>
		        val name = new LocalPath("ax" + num)
		        num = num+1
		        val args = c.getOperand
		        ApplySpine(OWLOMS("OWL2QL", "objectComplementOf"), classToLF(args) )
		        
		    case c : OWLObjectInverseOf =>
		        val name = new LocalPath("ax" + num)
		        num = num+1
		        val args = c.getInverse
		        ApplySpine(OWLOMS("OWL2QL", "objectInverseOf"), propertyToLF(args) )
		        
		   	//OWL2QL		    
		    //case c : OWLObjectAllValuesFrom =>
		    //case c : OWLObjectSomeValuesFrom =>
		        
		    //Arbitrary (Full) cardinality   OWL2SUB  
			//case c : OWLObjectExactCardinality =>
		    //case c : OWLObjectMaxCardinality =>
		    //case c : OWLObjectMinCardinality =>
		    
		    case c : OWLObjectOneOf =>
		        val name = new LocalPath("ax" + num)
		        num = num + 1
		        val args = c.getIndividuals.map(individualToLF)
		        ApplySpine(OWLOMS("OWL2QL", "objectOneOf"), args.toList : _*)
		    
		    //case c : OWLObjectHasValue =>
		    //case c : ObjectHasSelf =>
		}
	}
	
	def individualToLF (i : OWLIndividual) : Term = { 
		i match {
			case i : OWLNamedIndividual =>
		        CurrOMS(i.getIRI)
		}
 	}
	
	def propertyToLF (p : OWLObjectPropertyExpression ) : Term = {  
		p match {
			case p : OWLObjectProperty =>
				CurrOMS(p.getIRI)
		}
	}
	
	def objectToLF (o: OWLPropertyAssertionObject) : Term = {
		o match {
			
			case o : OWLNamedIndividual =>
				CurrOMS(o.getIRI)
				
		}
	}

	val OWL2 = new DPath(new jomdoc.utils.xml.URI("http://cds.omdoc.org/logics/description/owl/owl2.omdoc"))

	def OWLOMS (t: String, s: String) = {  
		OMS(OWL2 ? t ? s) 
	}

	def IRItoLocalPath(i: IRI) = new LocalPath(i.toString) 

	def CurrOMS(i: IRI) = OMS(currentThy ? IRItoLocalPath(i)) 

	def main(args: Array[String]) {
     
		    //Get hold of an ontology manager
			val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
			val controller = new Controller(NullChecker, new ConsoleReport)
		/*	
		    //Load an ontology from the web
			val iri : IRI = IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl");
			val pizzaOntology : OWLOntology  = manager.loadOntologyFromOntologyDocument(iri); 
			println("Loaded ontology: " + pizzaOntology);
			
			//Remove the ontology so that we can load a local copy.
			manager.removeOntology(pizzaOntology);
		
			//Load ontologies from files.  Download the pizza ontology from
			//http://www.co-ode.org/ontologies/pizza/pizza.owl and put it somewhere on your hard drive
			//Create a file object that points to the local copy
			val file : File = new File("E:\\Fall10\\CompSem\\Project\\ex2.owl");
			
			//Now load the local copy
			val test : OWLOntology  = manager.loadOntologyFromOntologyDocument(file);
			println("Loaded ontology: " + test);
		*/	
			
			val arg = args(0)
			val file : File = new File(arg)
			val test : OWLOntology  = manager.loadOntologyFromOntologyDocument(file)
			println("Loaded ontology: " + test)
						
			val documentIRI : IRI  = manager.getOntologyDocumentIRI(test)
			println("from: " + documentIRI)

			val dpath = DPath(new jomdoc.utils.xml.URI(documentIRI.toURI))
			val doc = new Document(dpath, controller)
			controller.add(doc)
		
		    //println("%" + "sig" + "ex" + "=" + "{" )
		    //%include OWL2SUB %open.
			
		    val thy = new Theory(dpath, LocalPath(List("_")), Some(OWL2 ? "OWL2Full"))
			controller.add(thy)
			currentThy = thy.path
			controller.add(MRef(dpath, currentThy, true))
		    
     		val axioms : Set[OWLAxiom] = test.getAxioms
			//lambda ax.F  ax => F
			axioms.foreach(ax => ax match {
				
				case ax : OWLDeclarationAxiom =>
				 	println(ax.getAxiomType)
				 	var entity : OWLEntity = ax.getEntity
				 	
				 	if(entity.isBuiltIn())
				 		   println(entity.toStringID())
				 		   
				 	else if(entity.isOWLAnnotationProperty())
					       println(entity.toStringID() + ":" + "annotationProperty" + "." )
						   					       
					else if(entity.isOWLClass) {
						   //println("old: " + entity.toStringID()+ " : " + "class" + "." )
						  
						   val name = IRItoLocalPath(entity.getIRI) // woman  
						   val tp  = OWLOMS("OWL2SUB", "class")     // type , module and name 
						   val con = new Constant(currentThy, name, Some(tp), None, null) 
						   //theory name: ex, class name:woman, type, none for definition  
						  
						   println(con.toString)
					} 
					
					else if(entity.isOWLDataProperty) {
				    	   //println("old: " + entity.toStringID() + " : " + "dataProperty" + "." )
				    	   
				    	   val name = IRItoLocalPath(entity.getIRI) 
						   val tp  = OWLOMS("OWL2SUB", "dataProperty")    
						   val con = new Constant(currentThy, name, Some(tp), None, null) 
						   controller.add(con)
						   
						   println(con.toString)
					}
				 	
				    else if(entity.isOWLDatatype) {
				    	   //println(entity.toStringID() + " : " + "dataType" + "." )
				    	   
				    	   val name = IRItoLocalPath(entity.getIRI) 
						   val tp  = OWLOMS("D2", "dataType")    
						   val con = new Constant(currentThy, name, Some(tp), None, null) 
						   controller.add(con)
						   
						   println(con.toString)
				    }   
						
				    else if(entity.isOWLNamedIndividual){
				    	   // println("old: " + entity.toStringID()+ " : " + "individual" + "." )
						   
				    	   val name = IRItoLocalPath(entity.getIRI)  // John
						   val tp  = OWLOMS("OWL2SUB", "individual") // type , module and name
						   val con = new Constant(currentThy, name, Some(tp), None, null)
				    	   controller.add(con)
						   
				    	   println(con.toString)
				    }
				    else if(entity.isOWLObjectProperty){
				    	   // println("old: " + entity.toStringID()+ " : " + "objectProperty" + "." )
				    	   
				    	   val name = IRItoLocalPath(entity.getIRI) // hasWife
						   val tp  = OWLOMS("OWL2SUB", "objectProperty")     // type , module and name
						   val con = new Constant(currentThy, name, Some(tp), None, null)
				    	   controller.add(con)
						   
				    	   println(con.toString)
				    }
				    else  println("none")   
				    
				/* en match {
				 	  case en : OWLIndividual =>
				 	     //ax.toString
				 	     // println(ax.name + " : " + "individual" + ".")
				 	    println("works")
				   }
				 */	     
				   
				case ax : OWLSubClassOfAxiom =>
				  //println(ax.getAxiomType)
				  //printClass(ax.getSubClass)
				  //printClass(ax.getSuperClass)
				  //println("_" + " : " + "(" + "subClassOf" + ax.getSubClass + ax.getSuperClass + ")" + ".")
				  
				  val name = new LocalPath("ax" + num) 
				  num = num+1
				  val tp = ApplySpine(OWLOMS("OWL2SUB", "subClassOf"), classToLF(ax.getSubClass), classToLF(ax.getSuperClass)) 
				  val con = new Constant(currentThy, name, Some(tp), None, null)
				  controller.add(con)
						   
				  println(con.toString)
			     
				 //Apply(Apply(f,a),b),  ApplySpine(f,a,b)
				 //con //p* //println
				  
				case ax : OWLSubObjectPropertyOfAxiom =>
				  // println("_" + " : " + "(" + "subObjectPropertyOf" + ax.getSubProperty + ax.getSuperProperty + ")" + ".")
				  
				  val name = new LocalPath("ax" + num) 
				  num = num+1
				  val tp = ApplySpine(OWLOMS("OWL2SUB", "subObjectPropertyOf"), propertyToLF(ax.getSubProperty), propertyToLF(ax.getSuperProperty))
				  val con = new Constant(currentThy, name, Some(tp), None, null)
				  controller.add(con)
						   
				  println(con.toString)
				   
				case ax : OWLObjectPropertyDomainAxiom =>
				  //println("_" + " : " + "(" + "objectPropertyDomain" + ax.getProperty + ax.getDomain + ")" + ".")
				  
				  val name = new LocalPath("ax" + num) 
				  num = num+1
				  val tp = ApplySpine(OWLOMS("OWL2SUB", "objectPropertyDomain"), propertyToLF(ax.getProperty), classToLF(ax.getDomain)) 
				  val con = new Constant(currentThy, name, Some(tp), None, null)
				  controller.add(con)
						   
				  println(con.toString)
				/*  
				case ax : OWLObjectPropertyRangeAxiom =>
			      //println("_" + " : " + "(" + "objectPropertyRange" + ax.getProperty + ax.getRange + ")" + ".")
				  
				  val name = new LocalPath("ax" + num)
			      num = num+1
				 //?R val tp = ApplySpine(OWLOMS("OWL2SUB", "objectPropertyRange"), propertyToLF(ax.getProperty), classToLF(ax.getRange))
				 // val con = new Constant(currentThy, name, Some(tp), None, null)
				 // controller.add(con)
						   
				 // println(con.toString)
				*/
			    				   
				case ax : OWLClassAssertionAxiom => 
				 /*println("class assertion")
				   printIndividual(ax.getIndividual)
			       printClass(ax.getClassExpression)
			       println("_" + " : " + "(" + "classAssertion" + ax.getClassExpression + ax.getIndividual + ")" + ".")
				 */
				   
				  val name = new LocalPath("ax" + num) 
				  num = num+1
				  val tp = ApplySpine(OWLOMS("OWL2SUB", "classAssertion"), classToLF(ax.getClassExpression), individualToLF(ax.getIndividual)) 
				  val con = new Constant(currentThy, name, Some(tp), None, null)
				  controller.add(con)
						   
				  println(con.toString)
				  
			    case ax : OWLObjectPropertyAssertionAxiom => 
				 /*println("property assertion")
				   printIndividual(ax.getSubject)
				   printProperty(ax.getProperty)
				   printObject(ax.getObject)
				 */   
				   // println("_" + " : " + "(" + "objectPropertyAssertion" + ax.getProperty + ax.getSubject + ax.getObject + ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "objectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), objectToLF(ax.getObject))
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
						   
				   println(con.toString)
				   
				case ax : OWLNegativeObjectPropertyAssertionAxiom => 
				 /*println("negativeObjectPropertyAssertion")
				   printIndividual(ax.getSubject)
				   printProperty(ax.getProperty)
				   printObject(ax.getObject)
				 */  
				   // println("_" + " : " + "(" + "negativeObjectPropertyAssertion" + ax.getProperty + ax.getSubject + ax.getObject + ")" + ".")
				  
				   val name = new LocalPath("ax" + num)
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "negativeObjectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), objectToLF(ax.getObject)) //?
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
						   
				   println(con.toString)
				      
			/*    case ax : OWLDataPropertyAssertionAxiom =>
				   println("dataPropertyAssertion")
				   printIndividual(ax.getSubject)
				   printProperty(ax.getProperty)
				   printObject(ax.getObject)
				   
				   println("_" + " : " + "(" + "dataPropertyAssertion" + ax.getProperty + ax.getSubject + ax.getObject + ")" + ".")
				  
				   val name = new LocalPath("ax" + num)
				   num = num+1 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "dataPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getIndividual), objectToLF(ax.getObject)) //?
                
                */
                
						 
			    case ax : OWLTransitiveObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "transitiveObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) //p*
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "transitiveObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
			    case ax : OWLSymmetricObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "symmetricObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "symmetricObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
			    case ax : OWLFunctionalObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "functionalObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num)
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "functionalObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
			    case ax : OWLInverseFunctionalObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "inverseFunctionalObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2QL", "inverseFunctionalObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				  
				case ax : OWLAsymmetricObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "asymmetricObjetProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "asymmetricObjetProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
				  
				case ax : OWLReflexiveObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "reflexiveObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "reflexiveObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				     
				case ax : OWLIrreflexiveObjectPropertyAxiom =>	
				   // println("_" + " : " + "(" + "irreflexiveObjectProperty" + ax.getProperty +  ")" + ".")
				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "irreflexiveObjectProperty"), propertyToLF(ax.getProperty)) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				     
				     
				case ax : OWLEquivalentClassesAxiom =>	
								  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getClassExpressionsAsList.map(classToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "equivalentClasses"), args : _*) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				  	  
				case ax : OWLDisjointClassesAxiom =>
				  				  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getClassExpressionsAsList.map(classToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "disjointClasses"), args : _*) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
				   
			    case ax : OWLEquivalentObjectPropertiesAxiom =>	
								  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getProperties.map(propertyToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "equivalentObjectProperty"), args.toList : _*)
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println(con.toString)
				   
				   
			    case ax : OWLDisjointObjectPropertiesAxiom =>
				  		  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getProperties.map(propertyToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "disjointObjectProperty"), args.toList : _*)
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println("new: " + con.toString)
				  
				  			  
				case ax : OWLInverseObjectPropertiesAxiom => 
				 /*println("inverseObjectProperties")
				   printProperty(ax.getFirstProperty)
				   printProperty(ax.getSecondProperty)
				 */  				   
				   //println("_" + " : " + "(" + "inverseObjectProperties" + ax.getFirstProperty + ax.getSecondProperty + ")" + ".")
				  
				   val name = new LocalPath("ax" + num)
				   num = num+1
				   val tp = ApplySpine(OWLOMS("OWL2QL", "inverseObjectProperties"), propertyToLF(ax.getFirstProperty), propertyToLF(ax.getSecondProperty)) //?
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
						   
				   println("new: " + con.toString)
				      
		        case ax : OWLDifferentIndividualsAxiom =>	
								  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getIndividualsAsList.map(individualToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "differentIndividuals"), args : _*)
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println("new: " + con.toString)
				  
				case ax : OWLSameIndividualAxiom =>	
								  
				   val name = new LocalPath("ax" + num) 
				   num = num+1
				   val args = ax.getIndividualsAsList.map(individualToLF) 
				   val tp = ApplySpine(OWLOMS("OWL2SUB", "sameIndividual"), args : _*) 
				   val con = new Constant(currentThy, name, Some(tp), None, null)
				   controller.add(con)
				   
				   println("new: " + con.toString)
				  
			    case _ => println("else")
			    
			    
			})
			
			println(controller.get(currentThy).toNode)
			println(controller.get(currentThy).toString)

	
		  /*
			// We can always obtain the location where an ontology was loaded from
			val documentIRI : IRI  = manager.getOntologyDocumentIRI(localPizza);
			println("    from: " + documentIRI);
			
			// Remove the ontology again so we can reload it later
			manager.removeOntology(pizzaOntology);
		

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
