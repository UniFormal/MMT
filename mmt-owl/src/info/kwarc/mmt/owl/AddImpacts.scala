package info.kwarc.mmt.owl
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata._
import info.kwarc.mmt.api.moc._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf._
import org.semanticweb.owlapi.model._
import scala.collection._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.URI

class AddImpacts(memory: ROMemory) extends ImpactPropagator(memory){
	
    /**
    * identifies impacts of a changed constant
    * @param path a path of a changed constant
    * @return paths of constants that are impacted by the changed constant
    */
	def dependsOn(path : Path) : Set[Path] = {
	    var impacts = new mutable.HashSet[Path]()
	    //memory.ontology.query(path,ToSubject(RefersTo)) (p => impacts += p)
	    //impacts
	//}
	    val gname = path match {
	 			 case CPath(parent: GlobalName, comp) => parent
	 			 case gn: GlobalName => gn
	    }
	 
	    val whichConstant = memory.content.getConstant(gname)
	    val isApplication = whichConstant.tp match{
	    					case Some(OMS(_)) => false
	    					case Some(_) => true
	    					case None => false //throw Exception("Not a Term!")
	    }
	    if(isApplication)
	     memory.ontology.query(path,ToObject(RefersTo)) (p => impacts += p) //axiom
	    else
	      memory.ontology.query(path,ToSubject(RefersTo)) (p => impacts += p) //entity
	    /*whichTerm match{
	    case OMS(s) =>  memory.ontology.query(path,ToSubject(RefersTo)) (p => impacts += p) //entity
	    case _ =>   memory.ontology.query(path,ToObject(RefersTo)) (p => impacts += p) //axiom 
	    }	         			
 	    */
	    println("impacts" + impacts)
	    impacts 
	    
	}	 
	
/*	    var whichTerm : Term = null
	    path match { 
       	case  CPath(parent : GlobalName, comp) =>   
       	      val whichConstant = memory.content.getConstant(parent)
        	  whichTerm = whichConstant.tp match {
    					  case Some(t) => t
    					  case None => throw Exception("Not a Term!")
       	 	  }
        case _ => None
	    }
	    whichTerm match { 
	    case OMS(s) =>  memory.ontology.query(path,ToSubject(RefersTo)) (p => impacts += p) //entity  
	    case _ =>   memory.ontology.query(path,ToObject(RefersTo)) (p => impacts += p) //axiom 
	    }	         			
 	    impacts 
	}
*/
    /**
    * adds an impact of each change to an impacted constant; adds metadata to an impacted constant
    * @param path path of an impacted constant
    * @param changes changes that have impacts on the constant
    * @return None
    */
	def propFunc(path : Path, changes : Set[ContentChange]) = {
	    println("path: "+ path + "changes" + changes)
	    val logic = DPath(URI("http","latin.omdoc.org") / "logics")
	    if (logic <= path.doc)
	      Nil
	    else {
	        path match {
		    case parent : GlobalName =>
			//case CPath(parent : GlobalName, comp) => 
			 	 val impactedConstant = memory.content.getConstant(parent)
			 	 val md = new MetaData
				 changes foreach {c =>
				    val mdatum = setMetaDatum(impactedConstant, c)
				    md.add(mdatum)
			 	 }
			 	 md.keys map {key =>
			 	    val old = impactedConstant.metadata.getValues(key)
				    val ud = UpdateMetadata(parent, key, old, md.getValues(key) ::: old)
				    println(ud)
				    ud
			 	 }
			case _ =>
			  println("nill")
			  Nil
			}
	  }
	}
	
	/**
    * sets metadatum for an impacted constant
    * @param an impacted constant, a change that have an impact on the impacted constant
    * @return metadatum about the change and its impact
    */
	def setMetaDatum(impCons: Constant, change: Change) : MetaDatum = {
	    val (changeType : String, changePath : GlobalName) = change match {
      														 case DeleteDeclaration(d) => ("delete", d.path)
															 case AddDeclaration(d)    => ("add", d.path)
														     //case rename ? => ("rename", d.path)
															 case UpdateComponent(path, name, old, nw) => ("update", path match { 
															      	  												 case path : GlobalName => path
															      	  												 case _ => None
															      		      									     })
															 }
    	val changedConstant : Constant =  memory.content.getConstant(changePath)
	    //set key for metadatum
		val name : String = impCons.tp match {
    						case Some(t) => t match { 
	      		     				   		case OMS(s) => changeType match { 
	      		     				   					   case "delete" => "possibleImpactByDelete"
	      		     				   		  		       case "add"    => "possibleImpactByAdd"
	      		     				   					   case "update" => "possibleImpactByUpdate"
	      		     				   					   }
	      		     				   		case _      => changeType match{ 
	      		     				   					   case "delete" => "impactByDelete"
	      		     				   					   //case "add"    => "impactByAdd"
	      		     				   					   case "rename" => "impactByRename"
	      		     				   					   }
	    							        }
    						case None => throw Exception("Not a Term!")
    						}
	    val base = impCons.tp match {
	      		   case Some(t) => t match { 
	      		     			   case OMS(s) => "http://omdoc.org/impact"
	      		     			   case _      => "http://omdoc.org/impact"
	      		   				   }
			       case None => throw Exception("Not a Term!")
	       	 	   }
    	val dpath = DPath(utils.URI(base))
	    val key : GlobalName = dpath ? "_" ? name 
	    //set value for metadatum
	    val value : Term = ApplySpine(OWL2OMS("OWL2SUB","literal"), OMSTR(setLiteral(impCons, changeType, changedConstant)), OWLOMS("OWL1Datatype","string"))
	    val md = new MetaDatum(key, value)
		md
	} 
	
	/**
    * sets a literal of type string for a value of metadatum
    * @param an impacted constant, a change type, a changed constant that have an impact on the impacted constant
    * @return literal of type string explaining the change and its impact
    */
	def setLiteral(impCons : Constant, changeType : String, changedConstant : Constant) : String = {
	    val changedConstantName : LocalName = changedConstant.name
	    val term : Term = (changedConstant.tp match { 
	                       case Some(t) => t
	                       case None => throw Exception("Not a Term!")
	    				   })
	    var entityType : String = " "				   
	        entityType = term match { 
	                     case OMS(p) => p.name.toString
	                     case _ => entityType
	    }	    				   
	            	    				   
	    impCons.tp match {
	    case Some(t) => t match { 
	    		     	case OMS(s) => setLiteralForImpactedConstantSymbol(changeType, changedConstantName)
	      		     	case _      => setLiteralForImpactedConstantApplication(changeType, changedConstantName, entityType)
	      		   		}
	    case None => throw Exception("Not a Term!")
	    }
	}
	
	/**
    * sets a literal of type string for a value of metadatum for the impacted constant that is type of symbol
    * @param a change type, the changed constant's name 
    * @return a literal of type string explaining the change and its impact
    */
	def setLiteralForImpactedConstantSymbol(chgType : String, chgdConsName : LocalName) : String ={
	  val str = chgType match { 
	      case "delete" => "An axiom with ID " + chgdConsName.toString + " that includes this entity has been deleted." 
	      case "add"=> "An axiom with ID " + chgdConsName.toString + " that includes this entity has been added."
	      case "update" => "An axiom with ID " + chgdConsName.toString + " that includes this entity has been updated." 
	      }
	  str
	 }
	
	/**
    * sets a literal of type string for a value of metadatum for the impacted constant that is type of application
    * @param a change type, the changed constant's name, the type of the changed constant
    * @return a literal of type string explaining the change and its impact
    */
	def setLiteralForImpactedConstantApplication(chgType : String, chgdConsName : LocalName, entityType : String) : String = {
	  val str = chgType match { 
	            case "delete" => "This axiom has been impacted by deleting " +  entityType + " " + chgdConsName.toString + "." 
	            // case "add"=> "This axiom has been impacted by adding " + entityType + chgdConsName.toString + "."
	            //case "rename" => This axiom has been impacted by renaming " + entityType + chgdConsName.toString + "." 
	            }
	  str
	}
	
}	

/*
<metadata>
<meta property="http://omdoc.org/impact/renaming?_?http:%2F%2Fomdoc.org%2Fimpact%2Frenaming">
 <om:OMOBJ xmlns:om="http://www.openmath.org/OpenMath">
  <om:OMA>
   <om:OMS module="OWL2SUB" name="literal" base="http://latin.omdoc.org/logics/description/owl/owl2.omdoc"></om:OMS>
   <om:OMSTR>This axiom has to be updated by renaming NamedIndividual John to James</om:OMSTR>
   <om:OMS module="OWL1Datatype" name="string" base="http://latin.omdoc.org/logics/description/owl/owl.omdoc"></om:OMS>
  </om:OMA>
 </om:OMOBJ>
</meta>
</metadata>
*/