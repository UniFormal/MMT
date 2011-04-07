package mizar.reader
import mizar.objects._
import mizar.reader._
import scala.xml._

object ArticleParser{
	/**
	 * parseArticle
	 * Format: Article
	 * @param n the entire article
	 */
	def parseArticle(n : Node) : Unit = n.label match {
		case "Article" => {
			val title = (n \ "@aid").text
			ParsingController.setArticle(title)
			var i = 0
			while(i < n.child.length) {
				parseArticleChildren(n.child(i))
				i += 1
			}
		}
	}
	/**
	 * parseArticleChildren
	 * Format: 
	 * @param n a child of article
	 */
	def parseArticleChildren(n : Node) : Unit =  n.label match {
		case "JustifiedTheorem" => {
			parseJustifiedTheorem(n)
		}
		case "DefinitionBlock" => {
			parseDefinitionBlock(n)
		}
		case "DefTheorem" => {
			parseDefTheorem(n)
		}
		case "Definiens" => {
			parseDefiniens(n)
		}
		case _ => {
			None
		}
	}
/**
	 * parseJustifiedTheorem
	 * Format: JustifiedTheorem -> (Proposition, Justification)
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseJustifiedTheorem(n : scala.xml.Node) : Unit = {
			val name : String = (n \ "@aid").text + "_thm_" + (n \ "@nr").text
			val prop : MizProposition = PropositionParser.parseProposition(n.child(0))
			//val just : MizJustification = JustificationParser.parseJustification(n.child(1))

			val j = new MizJustifiedTheorem(name, prop)
			ParsingController.addToArticle(j)
	}

	/**
	 * parseDefinitionBlock
	 * Format:
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseDefinitionBlock(n : Node) : Unit = {
		var assumptions : List[MizLet] = Nil
		var defs : List[XMLDefinition] = Nil
		var i : Int = 0
		var nSeq = n.child
		while (i < nSeq.length) {
			nSeq(i).label match {
				case "Let" => assumptions = ReasoningParser.parseLet(nSeq(i)) :: assumptions
				case "Definition" => {
					DefinitionParser.parseDefinition(nSeq(i)) match {
						case c : XMLConstructor => {
							val d = new XMLDefinition(c.aid, c.kind, c.relnr, c.symbolnr, c.argTypes, c.retType, assumptions)
							d.resolveArgs()
							defs = d :: defs
						}
						case _ => None
					}	
				}
				case _ => None 
			}
			i = i + 1
		}
		val dt = new XMLDefinitionBlock(defs)
		ParsingController.setDefBlock(dt)
	}

	
	/**
	 * parseDefTheorem
	 * Format:  Proposition;
	 * @param n  the node being parsed
	 * @return   the translated node
	 */
	def parseDefTheorem(n : scala.xml.Node) : Unit =  {
		val kind : String = (n \ "@kind").text
		val nr : Int = (n \ "@nr").text.toInt
		val constrkind : String = (n \ "@constrkind").text
		val constrnr : Int = (n \ "@constrnr").text.toInt
		val d = new XMLDefTheorem(kind, nr, constrkind, constrnr, PropositionParser.parseProposition(n.child(0)))
		//ParsingController.buildDefinition(d)
		d
	}
	/**
	 * parseDefiniens
	 * Format: Typ*, Essentials, Formula, ? DefMeaning
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseDefiniens(n : Node) : Unit = {
		val nr = (n \ "@nr").text.toInt
		val aid = (n \ "@aid").text
		val constrkind = (n \ "@constrkind").text
		val constrnr = (n \ "@constrnr").text.toInt
		
		parseDefMeaning(n.child(n.child.length - 1)) match {
			case tm : MizTerm => 
				val d = new XMLIsDefiniens(aid, nr, constrkind, constrnr, tm)
				ParsingController.addIsDefiniens(d)
			case fm : MizFormula => 
				val d = new XMLMeansDefiniens(aid, nr, constrkind, constrnr, fm)
				ParsingController.addMeansDefiniens(d)
		}
	}
	
	def parseDefMeaning(n : Node) : MizAny = {
		val node = n.child(n.child.length - 1)
		node.label match {
			case "Term" => {
				TypeParser.parseTerm(node)
			}
			case _ => {
				PropositionParser.parseFormula(node)
			}
		}
	}
}