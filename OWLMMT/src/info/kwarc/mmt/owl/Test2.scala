package info.kwarc.mmt.owl

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.File
import java.net.URI

import scala.collection.mutable.Set // 
import scala.collection.JavaConversions._ //
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
import info.kwarc.mmt.api.objects._  //import jomdoc.objects.{Term,OMS,OMI}
import info.kwarc.mmt.api.utils._

case class Exception(msg : String) extends java.lang.Throwable

object Test2 {
	var num : Int = 0
	val OWL = new DPath(utils.URI("http", "cds.omdoc.org") / "logics" / "description" / "owl" / "owl.omdoc") 
    val OWL2 = new DPath(utils.URI("http", "cds.omdoc.org") / "logics" / "description" / "owl" / "owl2.omdoc") 
	var currThy : MPath = null

	val checker = new FoundChecker(DefaultFoundation)
	val report = new FileReport(new java.io.File("controller.log"))
	val controller = new Controller(checker, report)
	//report("owl", "message")
		
	def printClass(c : OWLClassExpression ){println(c)}
	def printIndividual(i : OWLIndividual){println(i)}
	def printProperty[R <: OWLPropertyRange, P <: OWLPropertyExpression[R,P]](p : OWLPropertyExpression[R,P]){println(p)} 
	def printObject(o: OWLPropertyAssertionObject){println(o)}
	
	def number() : String = {
		num = num+1
		num.toString
	}
	
	def IRItoLocalPath(i: IRI) = {
	   val uri = utils.URI.fromJava(i.toURI)
	   val name = uri.fragment match {
	     case Some(s) => s
         case None => uri.path.last
	   }
		LocalName(name) 
	}
	
	def IRItoMMTURI(i: IRI) : MPath = {
				val dpath = DPath(utils.URI.fromJava(i.toURI)) 
				new MPath(dpath, LocalPath(List(i.toString + "?" + "_"))) // mmturi
	}
	
	def OWLOMS (m: String, s: String) = {  OMID(OWL ? m ? s) }
	def OWL2OMS (m: String, s: String) = {  OMID(OWL2 ? m ? s) }

	def CurrOMS(i: IRI) : Term = { OMID(currThy ? IRItoLocalPath(i))}
		
	def addConstant(name: LocalName, tp: Term) {
			val con = new Constant( OMMOD(currThy), name, Some(tp), None, null)
			//theory name: ex, class name:woman, type, none for definition
			controller.add(con)
			println(con.toString)
	}
		
	def classToLF(c : OWLClassExpression) : Term = {   
		c match {
			case c : OWLClass =>
		        CurrOMS(c.getIRI)
		        
			case c : OWLBooleanClassExpression => 
		    	c match {
					case c : OWLNaryBooleanClassExpression =>
		        		val (sig,dec) = c match {
		        			case c : OWLObjectIntersectionOf => ("OWL2SUB","objectIntersectionOf")    
		        			case c : OWLObjectUnionOf=> ("OWL2QL","objectUnionOf" )   
		        		}
		        		val args = c.getOperandsAsList.map(classToLF)
		        		ApplySpine(OWL2OMS(sig,dec), args : _*)
		    
					case c : OWLObjectComplementOf =>
		        		val arg = c.getOperand
		        		ApplySpine(OWL2OMS("OWL2QL", "objectComplementOf"), classToLF(arg) )
		     	}
		    	
		    //OWLQuantifiedRestriction 	
		    case c : OWLQuantifiedObjectRestriction => 
		        val (sig,dec) = c match {
		        	case c : OWLObjectAllValuesFrom => ("OWL2QL","objectAllValuesFrom")  // hasChild son  
		        	case c : OWLObjectSomeValuesFrom => ("OWL2EL","objectSomeValuesFrom") // hasPet dog  
		        }
		        val arg1 = c.getProperty
		        val arg2 = c.getFiller
		        ApplySpine(OWL2OMS(sig,dec), propertyToLF(arg1), classToLF(arg2))
		      	    
		    case c : OWLQuantifiedDataRestriction => 
		        val (sig,dec) = c match {
		        	case c : OWLDataAllValuesFrom => ("OWL2QL","dataAllValuesFrom")    //hasZIP integer
		        	case c : OWLDataSomeValuesFrom => ("OWL2EL","dataSomeValuesFrom")  
		        }
		        val arg1 = c.getProperty
		        val arg2 = c.getFiller
		        ApplySpine(OWL2OMS(sig,dec), propertyToLF(arg1), dataRangeToLF(arg2))
		               
     		/*   
		    case c : OWLObjectCardinalityRestriction => //Arbitrary (Full) cardinality
		    	val dec = c match {
		        	case c: OWLObjectExactCardinality => "objectExactCardinality" //4 hasChild
		        	case c: OWLObjectMinCardinality => "objectMinCardinality" 
		        	case c: OWLObjectMaxCardinality => "objectMaxCardinality"
		        	//objectMaxCardinality   : tm nonNegativeInteger -> objectProperty -> class. 
		        }
		        
//? intToLF ne olacak?
		        
		        val arg1 = c.getCardinality //non-negative integer 
		        val arg2 = c.getProperty
		       
		        if(c.isQualified) { 
		        	val arg3 = c.getFiller
		        	ApplySpine(OWL2OMS("OWL2SUB", dec  + "Qualified"), intToLF(arg1), propertyToLF(arg2), classToLF(arg3) ) //4 hasChild parent
		        } 
		        else 
		        	ApplySpine(OWL2OMS("OWL2SUB", dec), intToLF(arg1), propertyToLF(arg2))  
		      
		    case c : OWLDataCardinalityRestriction =>
		        val name = c match {
		        	case c: OWLDataExactCardinality => "dataExactCardinality"
		        	case c: OWLDataMinCardinality => "dataMinCardinality"
		        	case c: OWLDataMaxCardinality => "dataMaxCardinality"
		        }
		        val arg1 = c.getCardinality //non-negative integer 
		        val arg2 = c.getProperty
		        
		            if (c.isQualified){
		        	val arg3 = c.getFiller
		        	ApplySpine(OWL2OMS("OWL2SUB", dec  + "Qualified"), intToLF(arg1), propertyToLF(arg2), dataRangeToLF(arg3) ) 
		        }
		        else
		        	ApplySpine(OWL2OMS("OWL2SUB", dec), intToLF(arg1), propertyToLF(arg2))
		 */		        		     	
		           
	        case c : OWLObjectOneOf =>
		        val args = c.getIndividuals.map(individualToLF)
		        ApplySpine(OWL2OMS("OWL2SUB", "objectOneOf"), args.toList : _*)
		    
		    case c : OWLObjectHasValue =>
		    	val arg1 = c.getProperty
		        val arg2 = c.getValue
		        ApplySpine(OWL2OMS("OWL2SUB", "objectHasValue"), propertyToLF(arg1), individualToLF(arg2))
		        
				    	
		    case c : OWLObjectHasSelf =>
		        val arg = c.getProperty
		        ApplySpine(OWL2OMS("OWL2SUB", "objectHasSelf"), propertyToLF(arg))
		  }
	}
		
	def individualToLF (i : OWLIndividual) : Term = { 
		i match {
			case i : OWLNamedIndividual =>
		        CurrOMS(i.getIRI)
		        
		 /* case i : OWLAnonymousIndividual => i.getIDI */
		       
		}
	}

	def propertyToLF[R <: OWLPropertyRange, P <: OWLPropertyExpression[R,P]] (p : OWLPropertyExpression[R,P]) : Term = {  
		p match {
			case p : OWLObjectPropertyExpression =>
			p match {
				case p : OWLObjectProperty =>
					CurrOMS(p.getIRI)
				
				case p : OWLObjectInverseOf =>
		        val arg = p.getInverse()
		        ApplySpine(OWL2OMS("OWL2QL", "objectInverseOf"), propertyToLF(arg) )
			}
			
			case p : OWLDataPropertyExpression =>
			p match {
				case p : OWLDataProperty =>
					CurrOMS(p.getIRI)
		   }
		   
		}
	}
	
	def objectToLF (o: OWLPropertyAssertionObject) : Term = {
		o match {
			case o : OWLNamedIndividual =>
				CurrOMS(o.getIRI)
		}
	}

	def intToLF (i: Int) : Term = { OMI(i) }
	
	def literalToLF (lt: OWLLiteral) : Term = {
	    if (lt.isDouble())	
	    	OMF(lt.parseDouble())
	    else if (lt.isFloat())	
	    	OMF(lt.parseFloat())
	    else if (lt.isInteger()) 
	    	OMI(lt.parseInteger())
	    //else if (lt.isBoolean()) lt.parseBoolean()
	    /*
	    else if (lt.isRDFPlainLiteral)
	    {	if(lt.hasLang)
	    	{	lt.getLiteral()
	    		lt.getLang()
	    	}
	    	else	OMSTR(lt.getLiteral())
	    }
	    */
	    else if (lt.isRDFPlainLiteral)	OMSTR(lt.getLiteral())
	    else  throw Exception("none of the literals")
	    //literal OMSTR
	    //datatype attribution to remember which type		 		  
	
	}


	def facetToLF (f: OWLFacetRestriction) : Term = { null
		//val arg1 : OWLFacet = f.getFacet()
		//val arg2 : OWLLiteral = f.getFacetValue()
		//literalToLF(arg2)
		
	}
	
/* Buna gerek kalmadi - dataRangeToLF icinde zaten var
	def dataTypeToLF(d: OWLDatatype) : Term = {
			CurrOMS(d.getIRI)
	}
*/
	def dataRangeToLF(dr : OWLDataRange ) : Term = {
		dr match {
			case dr : OWLDatatype =>
			 	 //dr.getBuiltInDatatype() 
				 if(dr.isBoolean())             
					 OWLOMS("D1","boolean")
				 else if(dr.isDouble())
					 OWLOMS("D1","double")
				 else if(dr.isFloat())
					 OWLOMS("D1","float")
				 else if(dr.isInteger())
					 OWLOMS("D1","integer")
				 else if(dr.isRDFPlainLiteral())
					 OWL2OMS("D2","PlainLiteral")
				 else if(dr.isString())
					 OWLOMS("D1","string") 
				 else 
					 throw Exception("none of the data types")
			
				 //CurrOMS(dr.getIRI) buna gerek kalmadi sanirim?
			
			case dr : OWLNaryDataRange => 
			 	val dec = dr match {
			 		case dr : OWLDataIntersectionOf => "dataIntersectionOf"
			 		case dr : OWLDataUnionOf => "dataUnionOf"
			 	}
			 	val args = dr.getOperands.map(dataRangeToLF)
		        ApplySpine(OWL2OMS("OWL2SUB", dec), args.toList : _*)
			         
			case dr : OWLDataComplementOf =>
			 	val arg = dr.getDataRange()
		        ApplySpine(OWL2OMS("OWL2QL", "dataComplementOf"), dataRangeToLF(arg) )
		          
		    case dr : OWLDataOneOf =>  // "Peter" "1"^^xsd:integer 
			 	val args = dr.getValues.map(literalToLF)
			 	ApplySpine(OWL2OMS("OWL2SUB","dataOneOf"),args.toList : _*)
			 	   
		    case dr : OWLDatatypeRestriction =>  //xsd:integer xsd:minInclusive "5"^^xsd:integer xsd:maxExclusive "10"^^xsd:integer 
		    	val arg1 = dr.getDatatype
		    	val args = dr.getFacetRestrictions.map(facetToLF)
		    	ApplySpine (OWL2OMS("OWL2SUB","dataTypeRestriction"),  dataRangeToLF(arg1) :: args.toList : _*)
		}
	}
	
	
	
	def axiomToLF(ax : OWLAxiom) = { // def axiomToLF(ax : OWLAxiom) : Constant = {} constant donduruyor mu kontrol et
		ax match {
			
/*	if(isAnnotated()) AnnotationToLF(ax) */
// Declaration
				case ax : OWLDeclarationAxiom =>
				 	println(ax.getAxiomType)
				 	var entity : OWLEntity = ax.getEntity
				    val tp : Term = 
				    	if(entity.isBuiltIn())
				    		OWL2OMS("signame","BuiltIn") // have to extend this
				    	else if(entity.isOWLAnnotationProperty()) // comment
				    		OWL2OMS("OWL2SUB","annotationProperty") // type , module and name
				    	else if(entity.isOWLDataProperty) // hasAge
				    		OWL2OMS("OWL2SUB", "dataProperty")
				    	else if(entity.isOWLClass) // Woman 
				    		OWLOMS("OWLBase", "class")	
				    	else if(entity.isOWLDatatype) 
				    		OWLOMS("D1", "dataType")
				    	else if(entity.isOWLNamedIndividual)// John
				    		OWLOMS("OWLBase", "individual")
				    	else if(entity.isOWLObjectProperty) // hasWife
				    		OWLOMS("OWLBase", "objectProperty")
				    	else  throw Exception("none of the declaration axioms") 
				    	  
				    val name = IRItoLocalPath(entity.getIRI)
				    addConstant(name, tp)
				
// ClassAxiom	
				case ax : OWLClassAxiom =>
				    ax match {
				    	case ax : OWLSubClassOfAxiom =>
							val name = LocalName("ax" + number()) //num = num+1 
							val arg1 = ax.getSubClass()
							val arg2 = ax.getSuperClass()
							val tp = ApplySpine(OWLOMS("OWL2SUB", "subClassOf"), classToLF(arg1), classToLF(arg2)) 
							addConstant(name, tp)
				
					/*	case ax : OWLDisjointUnionAxiom =>  */
			
				    	case ax : OWLNaryClassAxiom =>	
							val (sig,dec) = ax match {
								case ax : OWLEquivalentClassesAxiom => ("OWL2SUB","equivalentClasses")
								case ax : OWLDisjointClassesAxiom => ("OWL2SUB","disjointClasses")
							} 
							val name = LocalName("ax" + number()) 
							val args = ax.getClassExpressionsAsList.map(classToLF)		
							val tp = ApplySpine(OWL2OMS(sig,dec), args : _*) 
							addConstant(name, tp) 
				    }	
				    
// ObjectPropertyAxiom
				   
				case ax : OWLSubObjectPropertyOfAxiom =>
				  // println("_" + " : " + "(" + "subObjectPropertyOf" + ax.getSubProperty + ax.getSuperProperty + ")" + ".")
				  val name = LocalName("ax" + number()) 
				  val arg1 = ax.getSubProperty()
				  val arg2 = ax.getSuperProperty()
				  val tp = ApplySpine(OWL2OMS("OWL2SUB", "subObjectPropertyOf"), propertyToLF(arg1), propertyToLF(arg2))
				  addConstant(name, tp)
				  
			//	case ax : OWLNaryPropertyAxiom => 
				  //OWLEquivalentObjectPropertiesAxiom, data , OWLDisjointObjectPropertiesAxiom,data, OWLInverseObjectPropertiesAxiom 

				case ax : OWLEquivalentObjectPropertiesAxiom =>	
			       val name = LocalName("ax" + number()) 
				   val args = ax.getProperties.map(propertyToLF) 
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "equivalentObjectProperty"), args.toList : _*)
				   addConstant(name, tp)
			 
			    case ax : OWLDisjointObjectPropertiesAxiom =>
				   val name = LocalName("ax" + number()) 
				   val args = ax.getProperties.map(propertyToLF) 
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "disjointObjectProperty"), args.toList : _*)
				   addConstant(name, tp)
				   
			    case ax : OWLInverseObjectPropertiesAxiom => 
				   val name = LocalName("ax" + number())
				   val arg1 = ax.getFirstProperty()
				   val arg2 = ax.getSecondProperty()
				   val tp = ApplySpine(OWL2OMS("OWL2QL", "inverseObjectProperties"), propertyToLF(arg1), propertyToLF(arg2)) //?
				   addConstant(name, tp)
				   
				case ax : OWLObjectPropertyDomainAxiom =>
				  //println("_" + " : " + "(" + "objectPropertyDomain" + ax.getProperty + ax.getDomain + ")" + ".")
				  val name = LocalName("ax" + number()) 
				  val tp = ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyDomain"), propertyToLF(ax.getProperty), classToLF(ax.getDomain)) 
			      addConstant(name, tp)
			      
			    // R? <- getRange
				case ax : OWLObjectPropertyRangeAxiom => //hasWife woman
				  val name =  LocalName("ax" + number())
			      val tp = ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyRange"), propertyToLF(ax.getProperty), classToLF(ax.getRange))
			   	  addConstant(name, tp)
			
				case ax : OWLObjectPropertyCharacteristicAxiom =>  
			    	val (sig,dec) = ax match {
						case ax : OWLFunctionalObjectPropertyAxiom => ("OWL2SUB","functionalObjectProperty")
						case ax : OWLInverseFunctionalObjectPropertyAxiom => ("OWL2QL","inverseFunctionalObjectProperty")
						case ax : OWLReflexiveObjectPropertyAxiom => ("OWL2SUB","reflexiveObjectProperty")
						case ax : OWLIrreflexiveObjectPropertyAxiom => ("OWL2SUB","irreflexiveObjectProperty")
						case ax : OWLSymmetricObjectPropertyAxiom => ("OWL2SUB","symmetricObjectProperty")
						case ax : OWLAsymmetricObjectPropertyAxiom => ("OWL2SUB","asymmetricObjetProperty")
						case ax : OWLTransitiveObjectPropertyAxiom => ("OWL2SUB","transitiveObjectProperty")
					}  
			    	val name = LocalName("ax" + number())
			    	val tp = ApplySpine(OWL2OMS(sig,dec), propertyToLF(ax.getProperty)) 
			    	addConstant(name, tp)
		
// DataPropertyAxiom
				   
			//OWLSubDataPropertyOfAxiom altinda OWLSubDataPropertyOfAxiom ve OWLSubObjectPropertyOfAxiom 
			    	
				case ax : OWLSubDataPropertyOfAxiom =>
			      	val name = LocalName("ax" + number()) 
				  	val arg1 = ax.getSubProperty()
				  	val arg2 = ax.getSuperProperty()
				  	val tp = ApplySpine(OWL2OMS("OWL2SUB", "subDataPropertyOf"), propertyToLF(arg1), propertyToLF(arg2))
				  	addConstant(name, tp)
				     
				case ax : OWLEquivalentDataPropertiesAxiom => 
			       	val name = LocalName("ax" + number()) 
				   	val args = ax.getProperties.map(propertyToLF) 
				   	val tp = ApplySpine(OWL2OMS("OWL2SUB", "equivalentDataProperty"), args.toList : _*)
				   	addConstant(name, tp)
				
				case ax : OWLDisjointDataPropertiesAxiom => //hasName  hasAddress
			    	val name = LocalName("ax" + number()) 
				    val args = ax.getProperties.map(propertyToLF) 
				    val tp = ApplySpine(OWL2OMS("OWL2SUB", "disjointDataProperties"), args.toList : _*)
				    addConstant(name, tp)
							
				case ax : OWLDataPropertyDomainAxiom =>
				    val name = LocalName("ax" + number()) 
				    val arg1 = ax.getProperty()
				    val arg2 = ax.getDomain
				    val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyDomain"), propertyToLF(arg1), classToLF(arg2)) 
				    addConstant(name, tp)
				   
				case ax : OWLDataPropertyRangeAxiom =>
					val name = LocalName("ax" + number())
					val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyRange"), propertyToLF(ax.getProperty), dataRangeToLF(ax.getRange))
			   	  	addConstant(name, tp)
 			
			
				case ax : OWLFunctionalDataPropertyAxiom => //hasAge
			   	  	val name = LocalName("ax" + number())
			    	val tp = ApplySpine(OWL2OMS("OWL2SUB","functionalDataProperty"), propertyToLF(ax.getProperty)) 
			    	addConstant(name, tp)
			 	   
// DatatypeDefinition

				case ax : OWLDatatypeDefinitionAxiom => // dataTypeDefinition : dataType -> dataRange -> type.
			  	val name = LocalName("ax" + number())
			   	val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataTypeDefinition"), dataRangeToLF(ax.getDatatype), dataRangeToLF(ax.getDataRange))
			    addConstant(name, tp)
// HasKey 
 			 
 			/*  case ax : OWLHasKeyAxiom =>  */
 
// Assertion
			   		    	
			    case ax : OWLClassAssertionAxiom =>	
			   		val name = LocalName("ax" + number())
			   		val tp = ApplySpine(OWL2OMS("OWL2SUB", "classAssertion"), classToLF(ax.getClassExpression), individualToLF(ax.getIndividual))
			    	addConstant(name, tp)
						    	
			    case ax : OWLNaryIndividualAxiom =>
			   		val (sig,dec) = ax match {
			   			case ax : OWLSameIndividualAxiom => ("OWL2SUB","sameIndividual")
			   			case ax : OWLDifferentIndividualsAxiom => ("OWL2SUB","differentIndividuals")
			   		}
			   		val name = LocalName("ax" + number())
			   		val args = ax.getIndividualsAsList.map(individualToLF)
			   		val tp = ApplySpine(OWL2OMS(sig,dec), args : _*)
			   		addConstant(name, tp)
			   
			    //OWLPeopertyAssertionAxiom
			   			/* OWLObjectPropertyAssertionAxiom, 
			   			   OWLNegativeObjectPropertyAssertionAxiom,
			   			   OWLDataPropertyAssertionAxiom, 
			   			   OWLNegativeDataPropertyAssertionAxiom */
			    case ax : OWLObjectPropertyAssertionAxiom => 
				   val name = LocalName("ax" + number()) 
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), objectToLF(ax.getObject))
				   addConstant(name, tp)
				   
				case ax : OWLNegativeObjectPropertyAssertionAxiom => 
				   val name = LocalName("ax" + number())
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "negativeObjectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), objectToLF(ax.getObject)) 
				   addConstant(name, tp)
				      
			 /*  case ax : OWLDataPropertyAssertionAxiom =>
				   val name = LocalName("ax" + number())
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), ?objectToLF(ax.getObject))
				   addConstant(name, tp)
			 	   
				  
				     
				case ax : OWLNegativeDataPropertyAssertionAxiom => 
				   val name = LocalName("ax" + number())
				   val tp = ApplySpine(OWL2OMS("OWL2SUB", "negativeDataPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), ?objectToLF(ax.getObject))
				   addConstant(name, tp)
								   
			  */
               
// AnnotationAxiom
							    
			 /* case ax : OWLAnnotationAssertionAxiom => 
			  * getAnnotation, getProperty, getSubject, getValue
			  * AnnotationAssertion( AP as av ) as: IRI or anonyind 
			  * AnnotationAssertion( rdfs:label a:Person "Represents the set of all people." ) */
				   

			 /*	case ax : OWLSubAnnotationPropertyOfAxiom => 
			  * getSubProperty, getSuperProperty
			  * SubAnnotationPropertyOf( AP1 AP2 ) */
				   
			 /*	case ax : OWLAnnotationPropertyDomainAxiom =>
			  * getDomain, getProperty
			  * AnnotationPropertyDomain( AP U ) */ 
			  
				
			 /* case ax : OWLAnnotationPropertyRangeAxiom  => 
			  * getProperty, getRange
			  * AnnotationPropertyRange( AP U ), U means IRI*/ 
				   
			    //Apply(Apply(f,a),b),  ApplySpine(f,a,b)
				   
				case _ => throw Exception("None of the axioms")	
			    
			    
		}
	}
/*	
    bu fonksiyonu cagirmadan once isAnnotated diye sor, oyle ise cagir
	def AnnotationToLF(ax : OWLAxiom) : Term = {
		val annots = ax.getAnnotations()
		annots.foreach { ann => 
			val annp: OWLAnnotationProperty = ann.getProperty() 
			if(annp.isBuiltIn())
			else if(annp.isComment())
			else if(annp.isDeprecated())
			else if(annp.isValue())
			
			val annv : OWLAnnotationValue = ann.getValue()
			val anna : OWLAnnotation = ann.getAnnotations()
			//isdeprecatedIRIAnnotation()
		}
	
	}	
*/
	def ontologyToLF(manager : OWLOntologyManager, controller : Controller, ontology : OWLOntology) {
	  
		val documentIRI : IRI  = manager.getOntologyDocumentIRI(ontology)
		println("from: " + documentIRI)
			
		val dpath = DPath(utils.URI.fromJava(documentIRI.toURI)) 
	    val document = new Document(dpath)
		controller.add(document)
		
		val theory = new DeclaredTheory(dpath, LocalPath(List("_")), Some(OWL2 ? "OWL2Full")) //constructor. doc, name, meta
		controller.add(theory)
		currThy = theory.path
		controller.add(MRef(dpath, currThy, true))
		
		//manager.method
		//java.util.Set<OWLOntology> 	getImportsClosure(OWLOntology ontology) 
		
		//java.util.List<OWLOntology> getSortedImportsClosure(OWLOntology ontology)
		//A list that represents a topological ordering of the imports closure. The first element in the list will be the specified ontology.
				
		//ontology.method
		//java.util.Set<OWLOntology> 	getDirectImports() 
		//java.util.Set<IRI> 	getDirectImportsDocuments() 
		
/* 2		val imports : java.util.List[OWLOntology] = manager.getSortedImportsClosure(ontology) // var type ?
		imports.foreach {imp =>  // first element is the ontology itself ?
				val from = IRItoMMTURI(imp.getOntologyID.getOntologyIRI())
				controller.add(PlainImport(from, currThy))
		}
		
*/
	
		// val imports : Set[IRI] = ontology.getDirectImportsDocuments()
		// imports.foreach { i => 
	    // cok onemli //if (not in controller zet) ontologyToLF(i)
        //      val from = IRItoMMTURI(i)
		//		controller.add(PlainImport(from, currThy))
		// }
			
				val axioms : Set[OWLAxiom] = ontology.getAxioms
				axioms.foreach(axiom => axiomToLF(axiom)) //axioms.foreach(axiom => controller.add(axiomToLF(axiom)))
	}

	def main(args: Array[String]) {
     
		    //Get hold of an ontology manager
			val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
			controller.handle(ExecFile(new java.io.File("startup.mmt"))) 

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
			*/
			
			//val file : File = new File("E:\\Fall10\\CompSem\\Project\\class.owl");
			//val file : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\OMDocOntology.owl");
			//val file : File = new File("E:\\Fall10\\CompSem\\Project\\deneme2.owl");
			//val file : File = new File("E:\\Fall10\\CompSem\\Project\\MMTtrunk\\implementation\\OWLMMT\\examples\\import.owl");
			val file : File = new File("examples\\ex2.owl");
			
			//Now load the local copy
		   val test : OWLOntology  = manager.loadOntologyFromOntologyDocument(file);
			println("Loaded ontology: " + test);
			
/*			val arg = args(0)
			val file : File = new File(arg)
			val test : OWLOntology  = manager.loadOntologyFromOntologyDocument(file)
			println("Loaded ontology: " + test)
*/
			
// 1		val imports : java.util.List[OWLOntology] = manager.getSortedImportsClosure(test) 
			
			//manager.getOntologies.foreach {onto => ontologyToLF(manager, controller, onto )} sor ?
			ontologyToLF(manager, controller, test )
			
		    //println("%" + "sig" + "ex" + "=" + "{" )
		    //%include OWL2SUB %open.
	
		/*	val axioms : Set[OWLAxiom] = test.getAxioms
			//lambda ax.F  ax => F
			axioms.foreach(ax => ax match {
				case ax : OWLDeclarationAxiom => ...
		*/	
			println(controller.get(currThy).toNode)
			println(controller.get(currThy).toString)

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

} //object Test2 nin parantezi
