package info.kwarc.mmt.api.webedit

import collection.mutable.HashMap
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.notations._
import scala.util.parsing.json._

class LanguageDictionary(controller : Controller) {
	//holds constant(globalName) languages notations
	lazy val consLang = getPairsOfLanguage()
	
	//FIXME : delete this.
	// Probably meant to add Mixfix(List(c.name.toString)) for using symbol local name as default english verbalization
	private def addNotation(lang : String, c : Constant) {
	  val not = new TextNotation(Mixfix(Nil), Precedence.integer(12), None, NotationScope(None, List(lang), 0))
	  c.notC.verbalizationDim.set(not)
	  controller.add(c)
	}
	
	/*
	 * returns a map[global name, map[language, list[text notations]]]
	 */
	def getPairsOfLanguage() = {
	  val paths = controller.depstore.getInds(ontology.IsConstant)
	  val constants = paths.map(x => controller.get(x)).toList.collect(x => x match{
	    case c: Constant => {
	      addNotation("en",c)
	      c.path -> c.notC.verbalizationDim.notations.values.toList.flatten
	    }
	  })
	  
	  val pairs = constants map {case(con,not) => con -> (not.flatMap(x => x.scope.languages).distinct -> not)}
	  val langMap = pairs map { case (con,(langs, not)) => {
	    val result = new collection.mutable.HashMap[String,List[TextNotation]]
	    langs foreach { x=> 
	      result(x) = not filter {notation => notation.scope.languages contains x}
	    }
	    con -> result.toMap
	  }}
	  langMap.toMap
	}
	
	//finds the constants that have ALL the listed languages
	def getConstByLanguage(language : List[String]) : List[GlobalName] = {
	  val filtered = consLang.toList filter {case(con,langMap) => !((language map {x => langMap.get(x) != None}) contains false)}
	  val result = filtered.map(_._1)
	  result
	}
	
	
	def getConstByLanguage(language : String) : List[GlobalName] = {
	  val filtered = consLang.toList filter {case(con,langMap) => langMap.get(language) != None}
	  val result = filtered.map(_._1)
	  result
	}
	
	//given a constant and the wanted language returns constant's translation if it exists
	def getTranslation(const : GlobalName, language : String) : Option[List[TextNotation]] = {
	  if(consLang.contains(const)){
		  val langMap = consLang(const)
		  langMap.get(language)
	  }else{
	    None
	  }
	}
	
	//matches the constant's toText method and translates it. There is a another method for matching markers
	def getTranslation(constant : String, language : String) : Option[List[TextNotation]] = {
	  val notation = TextNotation.parse(constant, NamespaceMap.empty)

	  val globalNameConst = findConstant(notation.toText, x => x.toText)
	  globalNameConst match {
	    case None => None
	    case Some(globalName) => getTranslation(globalName,language)
	  }
	}
	
	
	//translates the constant given its markers in strings
	def getTranslationByMarkers(constantMarkers : String, language : String) : Option[List[TextNotation]] = {
	  val notation = TextNotation.parse(constantMarkers, NamespaceMap.empty)
	  
	  def toStringMethod = (x: TextNotation) => x.markers.map(_.toString).mkString(" ")
	  
	  val globalNameConst = findConstant(toStringMethod(notation), toStringMethod)
	  globalNameConst match {
	    case None => None
	    case Some(globalName) => getTranslation(globalName,language)
	  }
	}
	
	//matches the TextNotation (toText) and finds the GlobalName of the constant
	def findConstant(notation : String, toStringMethod : TextNotation => String) : Option[GlobalName] = {
	  def stringMatching(notation: String, constantNotations : List[String], pos : Int) : Boolean = {
		  if(notation.length == pos){
		    return constantNotations != Nil
		  }
		  if(constantNotations == Nil){
		    return false
		  }
		  val filteredNotations = constantNotations filter {x => (x.length() > pos) && (x.charAt(pos) == notation.charAt(pos))}
		  stringMatching(notation, filteredNotations,pos + 1)
	  }
	  
	  val globalNameConst = consLang.toList find {case(constant,langNotationMap) 
		  											=>
		  											  val notations = langNotationMap.values.toList.flatten
		  											  stringMatching(notation, notations.map(x => toStringMethod(x)),0) 
		  											}
	  globalNameConst match {
	    case None => None
	    case Some(a) => Some(a._1)
	  } 
	}
	
	//extracting the markers and turning them into string
	  def toStringMethod(x: TextNotation) : String = {
	    val mks = x.markers.map {
	      case d : Delimiter => d.text
	      case m => m.toString
	    }
	    clean(mks.mkString(" "))
	  }
	  
	def getDictionary() = {
	  val languages = consLang.toList.flatMap({case(gl,x)=> x.toList.map({case(lang,not) => lang})}).distinct
	  var langmap = new HashMap[String,HashMap[String,HashMap[String,List[String]]]]()
	  languages.foreach(x => languages.foreach(y =>if(x!=y){
	    val globalNames = getConstByLanguage(List(x,y))
	    globalNames.foreach(gl => {
	      val translations = (getTranslation(gl,x),getTranslation(gl,y))
	      translations match{
	        case (None,_) =>  println(None) 
	        case (_, None) =>
	        case (Some(a),Some(b)) => if(a != Nil && b!= Nil){
	          a.foreach(textnotation1 => b.foreach( textnotation2 =>{
	        	  val lang1 = toStringMethod(textnotation1)
		          val lang2 = toStringMethod(textnotation2)
		          if(lang1 != "" && lang2 != ""){
				      if(!langmap.contains(x)){
				        langmap(x) = new HashMap[String,HashMap[String,List[String]]]()
				      }
				      if (!langmap(x).contains(y)) {
				        langmap(x)(y) = new HashMap[String, List[String]]()
				      }
				      if (!langmap(x)(y).contains(lang1)) {
				        langmap(x)(y)(lang1) = Nil
				      }
				      langmap(x)(y)(lang1) ::= lang2
		          }
	          }))
	          
	        }
	      }
	    })
	  }))
	  langmap
	}
	
	def clean(s : String) : String = {
     val seq : Seq[Char] = s.toIndexedSeq.flatMap {
       case '\n' => List(' ')
       case ',' => Nil
       case '"' => Nil
       case '[' | '{' | '}'| ']' => Nil
       case x => List(x)
     }
     seq.mkString
   }
	
	def getDefLinks() : JSONObject = {
	  val paths = controller.depstore.getInds(ontology.IsConstant)
	  val constants = paths.map(x => controller.get(x)).toList.collect(x => x match{
	    case c: Constant => {
	      c.notC.verbalizationDim.notations.values.toList.flatten map {n => 
	        clean(toStringMethod(n)) -> c.path.toPath
	      }
	    }
	  }).flatten.toMap
	  new JSONObject(constants)
	}
	
	
	def getDictionaryJSON() : JSONObject = {
	  val map = getDictionary()

	  new JSONObject(map.map({
	    case(key,value) => (key,new JSONObject(value.map({
	      case(k,v) => (k,new JSONObject(v.map({
	        case (k2,v2) => (k2,
	          new JSONArray(v2))
	      }).toMap
	      ))}).toMap
	      ))}).toMap)
	}
  
}