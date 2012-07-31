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
	def dependsOn(path : Path) : Set[Path] = {
	    val impacts = new mutable.HashSet[Path]() 
	    memory.ontology.query(path,ToSubject(RefersTo))(p => impacts += p) // B refersTo A
	    impacts //Bs
	}

	//path is ax168 (B), changes are As which impact B
	def propFunc(path : Path, changes : Set[ContentChange]) : Option[StrictChange] = {
		path match {
		case CPath(parent: GlobalName, comp) => 
			val constant = memory.content.getConstant(parent) 
			val mdatum = changes.map(setMetaDatum)
	 		constant.metadata.add(mdatum.toList:_*)
	 		None
		case _ => None
	   }
	}
	
	def setMetaDatum(change: Change) : MetaDatum = {
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
	    	case "add" => "add"
	    	//case "rename" => "rename"
	    	case "update" => "update"
	    }
	    val base = "http://omdoc.org/impact"
	    val dpath = DPath(utils.URI(base))
	    val key : GlobalName = dpath ? "_" ? name 
	    //set value for metadatum
	    val value : Term = ApplySpine(OWL2OMS("OWL2SUB","literal"), OMSTR(setLiteral(changeType, changedConstant)), OWLOMS("OWL1Datatype","string"))
	    val md = new MetaDatum(key, value)
		md
	} 

	def setLiteral(changeType: String, changedConstant: Constant) : String = {
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
	    val str = changeType match { 
	      case "delete" => "This axiom has been impacted by deleting " + entityType + changedConstantName.toString + "." 
	      case "add"=> "This axiom has been impacted by adding " + entityType + changedConstantName.toString + "."
	      //case "rename" => This axiom has been impacted by renaming " + entityType + changedConstantName.toString + "."
	      case "update" => "An axiom with id " + changedConstantName.toString + " including this entity has been updated."
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