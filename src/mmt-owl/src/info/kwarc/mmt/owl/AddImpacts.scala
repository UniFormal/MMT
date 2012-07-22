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

class AddImpacts{
	
	def propagate(cpath : CPath, changes : List[Change])(memory : ROMemory) = {
		val globalName = cpath.parent 
		val constant = memory.content.get(globalName) 
		val mdatum = changes.map(setMetaDatum)
 		constant.metadata.add(mdatum:_*)
	}

	def setMetaDatum(change: Change) : MetaDatum = {
	    var changePath : GlobalName = null 
	    val changeType : String = change match {
      		case DeleteDeclaration(d) => changePath = d.path
      								 	 "delete"
	      	case AddDeclaration(d)    => changePath = d.path
	    							     "add"
            //case rename => changePath = d.path
	      	//							  "rename"
	      	//case UpdateComponent(path, name, old, nw)
	    	}
	    val changedConstantName : LocalName = changePath.name
	    //set key for metadatum
		val name : String = changeType match {
	    	case "delete" => "delete"
	    	case "add" => "add"
	    	//case "rename" => "rename"
	    	//case "update" =>
	    }
	    val base = "http://omdoc.org/impact"
	    val dpath = DPath(utils.URI(base))
	    val key : GlobalName = dpath ? "_" ? name 
	    //set value for metadatum
	    val value : Term = ApplySpine(OWL2OMS("OWL2SUB","literal"), OMSTR(setLiteral(changeType, changedConstantName)), OWLOMS("OWL1Datatype","string"))
	    val md = new MetaDatum(key, value)
		md
	}                                   
		
	def setLiteral(changeType: String, changedConstantName: LocalName) : String = {
	    val str = changeType match { 
	      case "delete" => "This axiom is impacted by deleting " + changedConstantName.toString // what is it? a class? OMS name should be given
	      case "add"=> "This axiom is impacted by adding " + changedConstantName.toString
	      //case "rename" => This axiom is impacted by renaming " + changedConstantName.toString 
	      //case "update" =>
	      
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