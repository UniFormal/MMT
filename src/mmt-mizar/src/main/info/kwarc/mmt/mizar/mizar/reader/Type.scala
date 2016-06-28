package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._

object TypeParser {
	/**
	 * parseTyp
	 * Format: Typ
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseTyp(n: scala.xml.Node) : MizTyp = {
		val aid = (n \ "@aid").text
		val kind = (n \ "@kind").text
		val absnr = (n \ "@absnr").text.toInt
		val vid = (n \ "@vid").text
		val pid = (n \ "@pid").text
		val t = new MizTyp(aid, kind, absnr, vid, n.child.filter(x => x.label == "Cluster").map(parseCluster).toList, n.child.filter(x => x.label == "Term").map(parseTerm).toList, None)
		t.setName(ParsingController.resolveDef(aid, "M", absnr))
		t
	} 

	
	/**
	 * Format: ( Var | LocusVar | FreeVar | LambdaVar | Const | InfConst | Num | Func | PrivFunc | Fraenkel | QuaTrm | It | ErrorTrm )
	 * @param n the node to be parsed
	 * @return the parsed node
	 */
	def parseTerm(n : scala.xml.Node) : MizTerm = {
		n.label match {
			case "Var" => new MizVar((n \ "@nr").text.toInt)
			case "LocusVar" => new MizLocusVar((n \ "@nr").text.toInt)
			case "FreeVar" => new MizFreeVar((n \ "@nr").text.toInt)
			case "LambdaVar" => new MizLambdaVar((n \ "@nr").text.toInt)
			case "Const" => new MizConst((n \ "@nr").text.toInt, (n \ "@vid").text.toInt)
			case "InfConst" => new MizInfConst((n \ "@nr").text.toInt)
			case "Num" => new MizNum((n \ "@nr").text.toInt)
			case "Func" =>  {
				val aid = (n \ "@aid").text
				val kind = (n \ "@kind").text
				kind match {
				  case "F" =>
				    val nr = (n \ "@nr").text.toInt
				    val schemenr = (n \ "@schemenr").text.toInt
				    new MizSchemeFunc(aid, kind, nr, schemenr, n.child.map(parseTerm(_)).toList)
				  case _ => 
				    val absnr = (n \ "@absnr").text.toInt
					new MizFunc(aid, kind, absnr, n.child.map(parseTerm).toList) 
				}
			}
			case "ConstFunc" => {
			  new MizConstFunc((n \ "@nr").text.toInt, n.child.map(parseTerm).toList)
			}

			case "PrivFunc" => 
				val ls = n.child.map(parseTerm).toList
				new MizPrivFunc((n \ "@nr").text.toInt, ls.head, ls.tail)
			case "Fraenkel" =>
				val len = n.child.length
				val types = n.child.slice(0,len-2).map(parseTyp).toList
				val term = parseTerm(n.child(len-2))
				val form = PropositionParser.parseFormula(n.child(len-1))
				new MizFraenkel(types, term, form)
			case "QuaTrm" => 
			println("QuaTrm")
			new MizIt() //TODO
			case "It" =>
			println("It")
			new MizIt()
			case "Choice" => new MizChoice(parseTyp(n.child(0)))
			case "ErrorTrm" => new MizErrorTrm()
		}
	}
	
	def parseAdjective(n : Node) : MizAdjective = {
		val absnr = (n  \ "@absnr").text.toInt
		val aid = (n \ "@aid").text
		val kind = (n \ "@kind").text
		val pid = (n \ "@pid").text
		val terms = n.child.map(parseTerm).toList		
		val value = (n \ "@value").text match {
			case "false" => false
			case _ => true
		}
		
		new MizAdjective(aid, kind, absnr,pid, value, terms)
	}
		
	def parseCluster(n : Node) : MizCluster = {
		val adjs = n.child.map(parseAdjective).toList
		new MizCluster(adjs)
	}
 
}