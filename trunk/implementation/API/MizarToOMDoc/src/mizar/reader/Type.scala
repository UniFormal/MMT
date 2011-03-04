package mizar.reader
import mizar.objects._
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
		new MizTyp(aid, kind, absnr, Nil, n.child.filter(x => x.label == "Term").map(parseTerm).toList) //TODO
	}

	def parseRef(n : scala.xml.Node) : MizRef = {
		val nr : Int = (n \ "@nr").text.toInt
		val kind : String = (n \ "@kind").text
		val aid : String = (n \ "@aid").text
		val absnr : Int = (n \ "@absnr").text.toInt
		new MizRef(nr, kind, aid, absnr)
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
			case "Func" =>  new MizFunc((n \ "@aid").text, (n \ "@kind").text, (n \ "@absnr").text.toInt, n.child.map(parseTerm).toList) //TODO
			case "PrivFunc" => new MizIt() //TODO
			case "Fraenkel" => new MizIt() //TODO
			case "QuaTrm" => new MizIt() //TODO
			case "It" => new MizIt()
			case "Choice" => new MizChoice(parseTyp(n.child(0)))
			case "ErrorTrm" => new MizErrorTrm()
		}
	}
	
	
}