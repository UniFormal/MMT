package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._

object ReasoningParser {
	def parseLet(n : Node) : MizLet = {
		new MizLet((n \ "@nr").text.toInt, n.child.map(TypeParser.parseTyp).toList)
	}
	
	def parseConclusion(n : Node) : MizConclusion = {
		new MizConclusion() //TODO
	}
	
	def parseAssume(n : Node) : MizAssume = {
		new MizAssume(n.child.map(PropositionParser.parseProposition).toList)
	}
	
	def parseGiven(n : Node) : MizGiven = {
		val nr = (n \ "@nr").text.toInt
		val exSt = PropositionParser.parseProposition(n.child(0))
		val typs = n.child.filter(x => x.label == "Typ").map(TypeParser.parseTyp).toList
		val props = n.child.slice(1, n.child.length).filter(x => x.label == "Proposition").map(PropositionParser.parseProposition).toList
		new MizGiven(nr, exSt, typs, props)
	}
	
	def parseTake(n : Node) : MizTake = {
		new MizTake(TypeParser.parseTerm(n.child(0)))
	}
	
	def parseTakeAsVar(n : Node) : MizTakeAsVar = {
		new MizTakeAsVar((n \ "@nr").text.toInt, TypeParser.parseTyp(n.child(0)), TypeParser.parseTerm(n.child(1)))
	}
	
	def parseReasoning(n : Node) : MizReasoning = {
		new MizReasoning() //TODO
	}
	
}