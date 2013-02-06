package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Text

object DefinitionParser {
	def parseField(n : Node) : MizField = {
	  val aid = (n \ "@aid").text
	  val kind = (n \ "@kind").text
	  val absnr = (n \ "@absnr").text.toInt
	  new MizField(aid, kind, absnr)
	}
	
	def parseFields(n : Node) : List[MizField] = {
	  if (n.label == "Fields")
		  n.child.map(x => parseField(x)).toList
	  else 
	    throw ImplementationError("Parsing Error: Structure with no fields ")
	}
    
	def parseDefinition(n : Node, defBlockNr : Int) : MizAny = {
	  val kind = (n \ "@kind").text
	  val isRedef = ((n \ "@redefinition").text == "true")
	  val isExp = ((n \ "@expandable").text == "true")
	  var nCons = n.child.filter(_.label == "Constructor")
	  nCons.length match {
	    case 0 => // redefinition or expandable mode 
	      assert(isRedef || isExp, "DefinitionReader.parseDefinition -> case redef" +
	      		"Expected redefinition or expandable mode (due to missing constructor), but neither attributes were set")
	      val pattern = n.child.find(_.label == "Pattern").map(parsePattern) getOrElse {
	        throw ImplementationError("Definition Reader -> parseDefinition, missing pattern in def with no constructor")
	      }
	      new XMLDefinition(defBlockNr, kind, None, Some(pattern), isRedef, isExp)	      
	    case 1 => //normal definition
	      val constr = parseConstructor(nCons.head)
	      val patternO = n.child.find(_.label == "Pattern").map(parsePattern)
	      new XMLDefinition(defBlockNr, kind, Some(constr), patternO, isRedef, isExp)
	      
	    case nrCon if nrCon >= 3 => //structure definition
	      val xmlattr = parseConstructor(nCons(0))
		  val xmlstr = parseConstructor(nCons(1))
		  val fields = parseFields(nCons(1).child(nCons(1).child.length - 1))
		  val xmlaggr = parseConstructor(nCons(2))
		  val xmlsel = nCons.slice(3,nCons.length).map(parseConstructor)
		  
		  val selectors = xmlsel.map(x => new MizSelector(x.aid, x.nr, x.argTypes(0), x.retType)).toList
		   
		  selectors.zipWithIndex.map(p => ParsingController.selectors(p._1.aid) += (p._1.absnr -> (xmlstr.nr -> (p._2 + 1))))
		  ParsingController.attributes(xmlattr.aid) += (xmlattr.nr -> xmlstr.nr)
		   
		  val mstructs = xmlstr.argTypes
		  val args = xmlaggr.argTypes
		   
		  new MizStructDef(ParsingController.resolveDef(xmlstr.aid, "L", xmlstr.nr), xmlstr.aid, xmlstr.nr, 
		      args, mstructs, fields, selectors)		   

	    case i => throw new ImplementationError("Definition Reader -> parseDefinition found unexpected number of constructors : " + i)
	  }
	  
	}
	
	def parsePattern(n : Node) : XMLPattern = {
	  def orElse(s : String, default : String) : String = if (s != "") s else default
	  val aid = (n \ "@aid").text
	  val kind = (n \ "@kind").text
	  val nr = (n \ "@nr").text.toInt
	  val constrnr = (n \ "@constrnr").text.toInt
	  val formatnr = (n \ "@formatnr").text.toInt
	  val antonymic = (n \ "@antonymic").text == "true"
	  val constraid = orElse((n \ "@constraid").text, aid)
	  val constrkind = (n \ "@constrkind").text
	  val absconstrnr = orElse((n \ "@absconstrnr").text, "-1").toInt // if absconstrnr missing, then it is unnecessary (expandable mode)
	  val argTypes = n.child.find(_.label == "ArgTypes").map(_.child.map(TypeParser.parseTyp)) getOrElse {
	    throw ImplementationError("Pattern Reader -> parsePattern, missing ArgTypes child in Pattern elem")
	  }
	  val expO = n.child.find(_.label == "Expansion").map(x => TypeParser.parseTyp(x.child.head))
	  
	  new XMLPattern(aid, kind, nr, constrnr, antonymic, constraid, constrkind, absconstrnr, argTypes.toList, expO)
	}
	
	
	/*
	/**
	 * parseDefinition
	 */
	def pparseDefinition(n : Node) : MizAny = {
		 val kind = (n \ "@kind").text
		 var nCons = n.child.filter(x => x.label == "Constructor")
		 if (nCons.length == 1) {
			 var c = parseConstructor(nCons(0))
			 val pattern = n.child(n.child.length - 1)
			 pattern.label match {
				 case "Pattern" => 
				 	val fnr = (pattern \ "@formatnr").text.toInt
				 	//c.addSymbolNr(snr)
				 	ParsingController.dictionary.addAbsnr(c.kind, fnr, c.nr);
				 	ParsingController.dictionary.addPattern(c.kind, (pattern \ "@formatnr").text.toInt, c.aid);
				 	
				 case _ => 
			 }
			(n \ "@redefinition").text match {
				case "true" => 
			    	val redefaid = (nCons(0) \ "@redefaid").text
			    	val absredefnr = (nCons(0) \ "@absredefnr").text.toInt
			    	
					val d = new XMLRedefinition(c.aid, c.kind, c.nr, c.relnr, c.argTypes, c.retType, redefaid, absredefnr)
					d
				case _ =>  
					val d = new XMLDefinition(c.aid, c.kind, c.nr, c.relnr, c.argTypes, c.retType)
					d
			}

		 } else if (nCons.length >= 3) {
		   val xmlattr = parseConstructor(nCons(0))
		   val xmlstr = parseConstructor(nCons(1))
		   val fields = parseFields(nCons(1).child(nCons(1).child.length - 1))
		   val xmlaggr = parseConstructor(nCons(2))
		   val xmlsel = nCons.slice(3,nCons.length).map(parseConstructor)
		   
		   val selectors = xmlsel.map(x => new MizSelector(x.aid, x.nr, x.argTypes(0), x.retType)).toList
		   
		   selectors.zipWithIndex.map(p => ParsingController.selectors(p._1.aid) += (p._1.absnr -> (xmlstr.nr -> (p._2 + 1))))
		   ParsingController.attributes(xmlattr.aid) += (xmlattr.nr -> xmlstr.nr)
		   
		   val mstructs = xmlstr.argTypes
		   val args = xmlaggr.argTypes
		   
		   new MizStructDef(ParsingController.resolveDef(xmlstr.aid, "L", xmlstr.nr), xmlstr.aid, xmlstr.nr, args, mstructs, fields, selectors)		   
		 } else { //no constructor found, trying pattern (redef or expandable def)
		   n.child.find(x => x.label == "Pattern") match {
		     case Some(pattern) =>
		       val aid = (pattern \ "@aid").text
		       val nr = (pattern \ "@absconstrnr").text.toInt
		       val relnr = (pattern \ "@constrnr").text.toInt
		       val argTypes = pattern.child.filter(_.label == "ArgTypes").flatMap(_.child.map(TypeParser.parseTyp)).toList
               println(pattern)
		       if ((n \ "@redefinition").text == "true") { //redef
		         val redefaid = (pattern \ "@constraid").text
		         val absredefnr = (pattern \ "@absconstrnr").text.toInt
		         
		         new XMLRedefinition(aid, kind, nr, relnr, argTypes, None, redefaid, absredefnr)
		         //throw new java.lang.Error("adfsf")
		       } else { //TODO also check for expandable modes
		         new MizErr() 
		       }
		     case None => new MizErr() //giving up
		   }
		 }
	}
	*/
	
	/**
	 * parseConstructor
	 */
	def parseConstructor(n : Node) : XMLConstructor = {
		val aid = (n \ "@aid").text
		val kind = (n \ "@kind").text
		val nr = (n \ "@nr").text.toInt
		val relnr = (n \ "@relnr").text.toInt
		val retType = n.child.find(x => x.label == "Typ")
		val superfluous = (n \ "@superfluous").text != ""
		val absredefnrO = try {
		  Some((n \ "@absredefnr").text.toInt)
		} catch {
		  case _ : Throwable => None
		}
		val redefaid = (n \ "@redefaid").text
		val redefaidO = if (redefaid == "") None else Some(redefaid) 
		val args =  n.child.filter(x => x.label == "ArgTypes").flatMap(x => x.child.map(TypeParser.parseTyp)).toList  
		val retTypeO = retType match {
			case Some(rt) => Some(TypeParser.parseTyp(rt))
			case None => None
		}
		new XMLConstructor(aid, kind, nr, relnr, superfluous,args, retTypeO, redefaidO, absredefnrO)

	}
	
}