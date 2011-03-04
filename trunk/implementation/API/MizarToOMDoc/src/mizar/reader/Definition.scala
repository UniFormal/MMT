package mizar.reader
import mizar.objects._
import scala.xml._

object DefinitionParser {
	
	/**
	 * parseDefinition
	 */
	def parseDefinition(n : Node) : MizAny = {
		 var kind = (n \ "@kind").text
		 var nCons = n.child.filter(x => x.label == "Constructor")
		 if (nCons.length == 1) {
			 var c = parseConstructor(nCons(0))
			 c
		 } else {
			 new MizErr()
		 }
			 
	}
	
	/**
	 * parseConstructor
	 */
	def parseConstructor(n : Node) : MizConstructor = {
		val aid = (n \ "@aid").text
		var kind = (n \ "@kind").text
		var relnr = (n \ "@relnr").text.toInt
		var retType = n.child.find(x => x.label == "Typ")
		retType  match {
			case Some(rt) => new MizConstructor(aid, kind, relnr, n.child.filter(x => x.label == "ArgTypes").flatMap(x => x.child.map(TypeParser.parseTyp)).toList, Some(TypeParser.parseTyp(rt)))
			case None => new MizConstructor(aid, kind, relnr, n.child.filter(x => x.label == "ArgTypes").flatMap(x => x.child.map(TypeParser.parseTyp)).toList, None)
		}
	}
}