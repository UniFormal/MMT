package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._

object PropositionParser {
	/**
	 * parseFormula
	 * Format: (Not | And | For | Pred | PrivPred | Is | Verum | ErrorFrm )
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseFormula(n : scala.xml.Node) : MizFormula = n.label match {
		case "Not" => {
			new MizNot(parseFormula(n.child(0)))
		}
		case "And" => {
			new MizAnd(n.child.map(parseFormula).toList)
		}
		case "For" => {
			val typ = TypeParser.parseTyp(n.child(0))
			val sVid = (n \ "@vid").text
			val varName = ParsingController.resolveVar(sVid)
			new MizFor(varName, typ, parseFormula(n.child(1)))
		}
		case "Pred" => {
			val pid = (n \ "@pid").text
			val kind = (n \ "@kind").text
			val aid = (n \ "@aid").text
			val p = new MizPred(aid, (n \ "@kind").text, (n \ "@absnr").text.toInt, n.child.map(TypeParser.parseTerm).toList)
			p.setName(ParsingController.resolveDef(p.aid, p.kind, p.absnr))
			p
		}	
		case "PrivPred" => {
			new MizPrivPred((n \ "@nr").text.toInt, n.child.slice(0,n.child.length - 1).map(TypeParser.parseTerm).toList, parseFormula(n.child(n.child.length- 1)))
		}	
		case "Is" => {
			new MizIs(TypeParser.parseTerm(n.child(0)), TypeParser.parseTyp(n.child(1)))
		}
		case "Verum" => {
			new MizVerum()
		}
		case "ErrorFrm" => {
			new MizErrorFrm()
		} 
		case _ => 
			new MizErrorFrm() //TODO 
	}

	/**
	 * parseProposition
	 * Format:  Proposition -> Formula
	 * @param n  the node being parsed
	 * @return   the translated node
	 */
	def parseProposition(n : scala.xml.Node) : MizProposition = {
		val nr = (n \ "@propnr").text.toInt
		new MizProposition(nr, parseFormula(n.child(0)))	
	}
}