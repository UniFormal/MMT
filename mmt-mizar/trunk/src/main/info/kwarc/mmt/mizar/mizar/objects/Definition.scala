package info.kwarc.mmt.mizar.mizar.objects

/**
 * 	objects.Definition
 * 	Contains classes for handling Mizar Definitions	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */


//utils

class OldXMLDefinition(val aid : String, val kind : String, val nr : Int, val relnr : Int, val argTypes: List[MizTyp], 
    val retType : Option[MizTyp]) extends MizAny {
//	var prop : MizProposition = null
	var premises : List[MizLet] = Nil
	
//	def setProp(p : MizProposition) {
//		prop = p
//	}
	
	def setPremises(p : List[MizLet]) = {
		premises = p
	}
	
	def resolveArgs() : Option[List[(Option[String],MizTyp)]] = {
		val namedVars = premises.flatMap(x => x.types)
		if (namedVars.length == argTypes.length) {
			val names = namedVars.map(l => ParsingController.resolveVar(l.vid))
			Some(names.zip(argTypes))			
		} else {
			None
		}
	}
}

class OldXMLRedefinition(aid : String,  kind : String, nr : Int, relnr : Int, argTypes : List[MizTyp], retType : Option[MizTyp], 
    val constraid : String, val constrabsnr : Int) 
  extends OldXMLDefinition(aid, kind, nr, relnr, argTypes, retType)


class XMLDefinition(val defBlockNr : Int, val kind : String, val constr : Option[XMLConstructor],
    val pattern : Option[XMLPattern], val isRedef : Boolean, val isExp : Boolean)
  extends MizAny {
  
  var definiens : Option[XMLDefiniens] = None
  var premises : List[MizLet] = Nil
  
  def setDefiniens(df : Option[XMLDefiniens]) = definiens = df
  
  private def resolveArgs(argTypes : List[MizTyp]) : Option[List[(Option[String],MizTyp)]] = {
    val namedVars = premises.flatMap(x => x.types)
    if (namedVars.length == argTypes.length) {
      val names = namedVars.map(l => ParsingController.resolveVar(l.vid))
      Some(names.zip(argTypes))			
    } else {
      None
    }
  }
  
  def args = {
    val tmpArgs = constr match {
      case Some(c) => c.argTypes
      case None => pattern match {
        case Some(s) => s.argTypes
        case None => definiens match { 
          case Some(df) => df.args.init //all elements except last
          case None => throw new Error("XMLDefinition.args for definition with no definiens and no pattern")
        }
      }
    }
    resolveArgs(tmpArgs)
  }
  
  def retType = constr match {
    case Some(c) => c.retType match {
      case Some(rt) => rt
      case None => throw new Error("XMLDefinition.retType for definition with constructor with no return type")
    }
    case None => definiens match {
      case Some(df) => df.args.last
      case None => throw new Error("XMLDefinition.retType for definition with no constructor and no definiens")
    }
  }
  
  def retTypeO = {
    try {
      Some(retType)
    } catch {
      case _ : Throwable => None
    }
  }
  
  
  def name = ParsingController.resolveDef(constraid, constrkind, absconstrnr)
  
  
  def isSuperfluous : Boolean = constr match {
    case Some(c) => c.superfluous
    case None => false
  }
  
  val constraid = constr match {
    case Some(c) => c.aid
    case None => pattern match {
      case Some(p) => p.constraid
      case None => throw new Error("XMLDefinition.constraid in def without constructor and pattern")
    }
  }
  
  val constrkind = constr match {
    case Some(c) => c.kind
    case None => pattern match {
      case Some(p) => p.constrkind
      case None => kind
    }
  }
  
  val absconstrnr = constr match {
    case Some(c) => c.nr
    case None => pattern match {
      case Some(p) => p.absconstrnr
      case None => throw new Error("XMLDefinition.absconstrnr in def without constructor and pattern")
    }
  }  
  
  val constrnr = constr match {
    case Some(c) => c.relnr
    case None => pattern match {
      case Some(p) => p.constrnr
      case None => throw new Error("XMLDefinition.constrnr in def without constructor and pattern")
    }
  }
  
  def getExpansion : MizTyp = pattern match {
    case Some(p) => p.exp match {
      case Some(t) => t
      case None => throw new Error("XMLDefinition.getExpansion but pattern has no expansion")
    }
    case None => throw new Error("XMLDefinition.getExpansion but no pattern found")
  }
  
  
  def patternNr = pattern match {
    case Some(p) => p.nr
    case None => throw new Error("XMLDefinition.patternNr but not pattern found")
  }
  
  def aid = pattern match {
    case Some(p) => p.aid
    case None => definiens match {
      case Some(d) => d.constraid
      case None => throw new Error("XMLDefinition.aid in def without pattern and definiens")
    }
  }
  
  def defaid = (isRedef, constr) match {
    case (true, Some(c)) => c.redefaid.getOrElse(throw new Error("XMLDefinition.defaid for no constructor and no redefaid"))
    case _ => constraid
  }
  
  def defnr = (isRedef, constr) match {
    case (true, Some(c)) => c.absredefnr.getOrElse(throw new Error("XMLDefinition.defnr for no constructor and no redefnr"))
    case _ => absconstrnr
  }
}

class XMLPattern(val aid : String, val kind : String, val nr : Int, val constrnr : Int, val antonymic : Boolean,
    val constraid : String, val constrkind : String, val absconstrnr : Int, val argTypes : List[MizTyp], val exp : Option[MizTyp])

class XMLConstructor(val aid : String, val kind : String, val nr : Int, val relnr: Int, 
    val superfluous : Boolean, val argTypes: List[MizTyp], val retType: Option[MizTyp],
    val redefaid : Option[String], val absredefnr : Option[Int]) 
  extends MizAny



class MizDefTheorem(val aid : String, val kind : String, val nr : Int, val constrkind : String, val constrnr : Int, val prop : MizProposition)

class XMLDefiniens(val aid : String, val nr : Int, val constraid : String, 
    val constrkind : String, val absconstrnr : Int, val args : List[MizTyp])

class XMLIsDefiniens(aid : String, nr : Int, constraid : String, constrkind : String, absconstrnr : Int, 
    val cases : List[(MizTerm, MizFormula)], val term : Option[MizTerm], args : List[MizTyp]) 
  extends XMLDefiniens(aid,nr, constraid, constrkind, absconstrnr, args)

class XMLMeansDefiniens( aid : String,  nr : Int,  constraid : String,  constrkind : String,  absconstrnr : Int,
    val cases : List[(MizFormula, MizFormula)], val form : Option[MizFormula], args : List[MizTyp]) 
  extends XMLDefiniens(aid,nr, constraid, constrkind, absconstrnr, args)

trait XMLDefMeaning

class XMLIsDefMeaning(val term: MizTerm) extends XMLDefMeaning
class XMLMeansDefMeaning(val form : MizFormula) extends XMLDefMeaning

