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

class AddImpacts(memory: ROMemory) extends ImpactPropagator(memory){
	// path is Man (A), B depends on A
    // changed constant
	def dependsOn(path : Path) : Set[Path] = {
	    var impacts = new mutable.HashSet[Path]()
	    var whichTerm : Term = null
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
	        case OMS(s) =>  memory.ontology.query(path,ToSubject(RefersTo)) (p => impacts += p) // entity // B refersTo A 
	        case _ =>   memory.ontology.query(path,ToObject(RefersTo)) (p => impacts += p) // axiom 
	  }	         			
 	   impacts //Bs
	}

	//path is ax168 (B), changes are As which impact B
	//impacted constant, set of changes that have impact on the constant
	def propFunc(path : Path, changes : Set[ContentChange]) : Option[StrictChange] = {
		path match {
		case CPath(parent : GlobalName, comp) => 
			val impactedConstant = memory.content.getConstant(parent) 
			val mdatum = changes.map(c => setMetaDatum(c, impactedConstant)) //val mdatum = changes.map(setMetaDatum)
	 		impactedConstant.metadata.add(mdatum.toList:_*)
	 		None
		case _ => None
	   }
	}
	
	def setMetaDatum(change: Change, impCons: Constant) : MetaDatum = {
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
		val name : String = changeType match {
	    	case "delete" => "delete"
	    	case "add"    => "add"
	    	//case "rename" => "rename"
	    	case "update" => "update"
	    }
	    val base = "http://omdoc.org/impact"
	    val dpath = DPath(utils.URI(base))
	    val key : GlobalName = dpath ? "_" ? name 
	    //set value for metadatum
	    val value : Term = ApplySpine(OWL2OMS("OWL2SUB","literal"), OMSTR(setLiteral(changeType, changedConstant, impCons)), OWLOMS("OWL1Datatype","string"))
	    val md = new MetaDatum(key, value)
		md
	} 

	def setLiteral(changeType : String, changedConstant : Constant, impCons : Constant) : String = {
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
	
	def setLiteralForImpactedConstantSymbol(chgType : String, chgdConsName : LocalName) : String ={
	  val str = chgType match { 
	      case "delete" => "An axiom with ID " + chgdConsName.toString + " that includes this entity has been deleted." 
	      case "add"=> "An axiom with ID " + chgdConsName.toString + " that includes this entity has been added."
	      case "update" => "An axiom with ID " + chgdConsName.toString + " that includes this entity has been updated." // think about it
	      }
	    str
	 }
	
	def setLiteralForImpactedConstantApplication(chgType : String, chgdConsName : LocalName, entityType : String) : String = {
	  val str = chgType match { 
	      case "delete" => "This axiom has been impacted by deleting " +  entityType + chgdConsName.toString + "." 
	      case "add"=> "This axiom has been impacted by adding " + entityType + chgdConsName.toString + "."
	      //case "rename" => This axiom has been impacted by renaming " + entityType + chgdConsName.toString + "." // update it
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