package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._
import info.kwarc.mmt.api._

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
    
	/**
	 * parseDefinition
	 */
	def parseDefinition(n : Node) : MizAny = {
		 var kind = (n \ "@kind").text
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
			    	
					val d = new XMLRedefinition(c.aid, c.kind, c.nr, c.argTypes, c.retType, redefaid, absredefnr)
					d
				case _ =>  
					val d = new XMLDefinition(c.aid, c.kind, c.nr, c.argTypes, c.retType)
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
		 } else {
			 new MizErr()
		 }
			 
	}
	
	/**
	 * parseConstructor
	 */
	def parseConstructor(n : Node) : XMLConstructor = {
		val aid = (n \ "@aid").text
		var kind = (n \ "@kind").text
		var nr = (n \ "@nr").text.toInt
		var retType = n.child.find(x => x.label == "Typ")
		
		retType  match {
			case Some(rt) => new XMLConstructor(aid, kind, nr, n.child.filter(x => x.label == "ArgTypes").flatMap(x => x.child.map(TypeParser.parseTyp)).toList, Some(TypeParser.parseTyp(rt)))
			case None => new XMLConstructor(aid, kind, nr, n.child.filter(x => x.label == "ArgTypes").flatMap(x => x.child.map(TypeParser.parseTyp)).toList, None)
		}
	}
	
}