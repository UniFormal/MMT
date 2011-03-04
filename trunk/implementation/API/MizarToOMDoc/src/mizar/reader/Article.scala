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
		var defs : List[MizDefinition] = Nil
		var i : Int = 0
		var nSeq = n.child
		while (i < nSeq.length) {
			nSeq(i).label match {
				case "Let" => assumptions = ReasoningParser.parseLet(nSeq(i)) :: assumptions
				case "Definition" => {
					DefinitionParser.parseDefinition(nSeq(i)) match {
						case c : MizConstructor => {
							val d = new MizDefinition(c.aid, c.kind, c.relnr, c.argTypes, c.retType, assumptions)
							defs = d :: defs
						}
						case _ => None
					}	
				}
				case _ => None 
			}
			i = i + 1
		}
		val dt = new MizDefinitionBlock(defs)
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
		val d = new MizDefTheorem(kind, nr, constrkind, constrnr, PropositionParser.parseProposition(n.child(0)))
		ParsingController.buildDefinition(d)
	}
	/**
	 * parseDefiniens
	 * Format: Typ*, Essentials, Formula, ? DefMeaning
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseDefiniens(n : scala.xml.Node) : MizDefiniens = {
		val constrnr = (n \ "@constrnr").text.toInt
		val form = n.child.find(x => x.label == "Not" || x.label == "And" || x.label == "For")//TODO
		form match {
			case None => new MizDefiniens()//(constrnr, ErrorFrm())
			case Some(f) => new MizDefiniens()//(constrnr, parseFormula(f))
		}
	}
}