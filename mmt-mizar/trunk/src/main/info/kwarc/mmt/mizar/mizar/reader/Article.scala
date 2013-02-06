package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
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
			ParsingController.currentAid = title
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
		case "JustifiedTheorem" =>
			parseJustifiedTheorem(n)
		case "DefinitionBlock" =>
			parseDefinitionBlock(n, ParsingController.defBlockCounter)
		case "DefTheorem" =>
			parseDefTheorem(n)
		case "Definiens" =>
			parseDefiniens(n)
		case "NotationBlock" =>
			parseNotationBlock(n)
		case "SchemeBlock" =>
		    SchemeRegReader.parseSchemeBlock(n)
		case "Proposition" => 
		  parseLemma(n)
		case "RegistrationBlock" => 
		  SchemeRegReader.parseRegistrationBlock(n)
		case "Set" | "Consider" | "Reconsider" | "Now" | "DefPred" |"DefFunc"| "IterEquality" => 
		  ParsingController.addToArticle(JustificationParser.parseAuxiliaryItem(n))		
		case "Proof" | "By" | "From" | "Reservation" |"Section"  => None
		case _ => {
			println("TODO: " + n.label)
		}
	}
	
    /**
	 * parseJustifiedTheorem
	 * Format: JustifiedTheorem -> (Proposition, Justification)
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseJustifiedTheorem(n : scala.xml.Node) : Unit = {
	  val aid = (n \ "@aid").text
	  val nr = (n \ "@nr").text.toInt
	  val propNode = n.child(0)
	  val prop : MizProposition = PropositionParser.parseProposition(propNode)
	  val just : MizJustification = JustificationParser.parseJustification(n.child(1))

	  val j = new MizJustifiedTheorem(aid, nr, prop, just)
	  // position of proposition inside justified theorem happens to be the
	  // end of the proposition instead of beginning. possibly a bug in mizxml but we can use it
	  val end = UtilsReader.parseSourceRef(propNode)
	  // jts don't have source refs in version 1132 but do in 1147, must check for that
	  val start = try {
	    UtilsReader.parseSourceRef(n)
	  } catch {
	    case e : Throwable => end
	  }
	  j.sreg = Some(SourceRegion(start, end))
	  ParsingController.addToArticle(j)
	}

	/**
	 * parseDefinitionBlock
	 * Format:
	 * @param n the node being parsed
	 * @return the translated node
	 */
	def parseDefinitionBlock(n : Node, defBlockNr : Int) : Unit = {
		var assumptions : List[MizLet] = Nil
		var defs : List[XMLDefinition] = Nil
		var nSeq = n.child
		val start = UtilsReader.parseSourceRef(n)
		val end : SourceRef = UtilsReader.parseSourceRef(n.child.last)
		val sreg = Some(SourceRegion(start,end))
		nSeq foreach { node => 
			node.label match {
				case "Let" => assumptions = JustificationParser.parseLet(node) :: assumptions
				case "Definition" => {
					DefinitionParser.parseDefinition(node, defBlockNr) match {
						case d : XMLDefinition => {
							d.premises = assumptions
							//TODO d.resolveArgs() 
							d.sreg = sreg
							defs = d :: defs
						}
						case s : MizStructDef => {
						  s.sreg = sreg
						  ParsingController.addToArticle(s)
						}
						case _ => None
					}	
				}
				case _ => None 
			}
		}		
		val dt = new XMLDefinitionBlock(defs.reverse)
		dt.sreg = Some(SourceRegion(start,end))
		ParsingController.addDefinitionBlock(dt)
	}

	
	/**
	 * parseDefTheorem
	 * Format:  Proposition;
	 * @param n  the node being parsed
	 * @return   the translated node
	 */
	def parseDefTheorem(n : scala.xml.Node) : Unit =  {
		try {
	      val kind : String = (n \ "@kind").text
		  val nr : Int = (n \ "@nr").text.toInt
		  val constrkind : String = (n \ "@constrkind").text
	      val constrnr : Int = (n \ "@constrnr").text.toInt
	      val aid : String = (n \ "@aid").text
		  val d = new MizDefTheorem(aid, kind, nr, constrkind, constrnr, PropositionParser.parseProposition(n.child(0)))
		  ParsingController.addDefTheorem(d)
		} catch {
		  case e : java.lang.NumberFormatException => None // means cancelled definition, can be ignored
		}
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
		val constraid = (n \ "@constraid").text
		val constrkind = (n \ "@constrkind").text
		val absconstrnr = (n \ "@absconstrnr").text.toInt
		val args = n.child.filter(_.label=="Typ").map(TypeParser.parseTyp).toList
		
		val pair = parseDefMeaning(n.child(n.child.length - 1))
		pair._1 match {
			case Some(eDef) => 
				val d = new XMLIsDefiniens(aid, nr, constraid, constrkind, absconstrnr, eDef._1, eDef._2, args)
				ParsingController.addDefiniens(d)
			case None => pair._2 match {
				case Some(mDef) => 
				val d = new XMLMeansDefiniens(aid, nr, constraid, constrkind, absconstrnr, mDef._1, mDef._2, args)
				ParsingController.addDefiniens(d)
				case None => None
			}
			
				
		}
	}
	
	def parseDefMeaning(n : Node) : (Option[(List[(MizTerm, MizFormula)],Option[MizTerm])],Option[(List[(MizFormula, MizFormula)],Option[MizFormula])]) = {
		(n \ "@kind").text match {
			case "e" => {
				val cases : List[(MizTerm, MizFormula)] = 
					n.child.filter(x => x.label == "PartialDef").map(x =>TypeParser.parseTerm(x.child(0)) -> PropositionParser.parseFormula(x.child(1)) ).toList
				val last = n.child(n.child.length - 1)
				val default = last.label match {
					case "PartialDef" => None
					case _ => Some(TypeParser.parseTerm(last))
				}
				(Some(cases -> default),None)
				
			}
			case "m" => {
				val cases : List[(MizFormula, MizFormula)] = 
					n.child.filter(x => x.label == "PartialDef").map(x =>PropositionParser.parseFormula(x.child(0)) -> PropositionParser.parseFormula(x.child(0))).toList
				val last = n.child(n.child.length - 1)
				val default = last.label match {
					case "PartialDef" => None
					case _ => Some(PropositionParser.parseFormula(last))
				}
				(None, Some(cases -> default))	
			}
		}
	}

	
	def parseNotationBlock(n : Node) {
	    val start = UtilsReader.parseSourceRef(n)
	    val end = UtilsReader.parseSourceRef(n.child.last)
	    val sreg = Some(SourceRegion(start,end))
	    
	    var notations = n.child.filter(_.label == "Pattern").map(parseNotationPattern)
	    notations foreach {not => 
	      not.sreg = sreg 
	      ParsingController.addToArticle(not)
	    }
	}
	
	def parseNotationPattern(n : Node) : MizNotation = {
		val aid = (n \ "@aid").text
		val kind = (n \ "@kind").text
		val constrAid = (n \ "@constraid").text
		val antonymic = (n \ "@antonymic").text match {
			case "true" => true
			case _ => false
		}
		val absconstrnr = (n \ "@absconstrnr").text.toInt
		val formatnr = (n \ "@formatnr").text.toInt
		val nr = (n \ "@nr").text.toInt
		val relnr = (n \ "@relnr").text.toInt
		
		ParsingController.dictionary.addPattern(kind, formatnr, aid)
		new MizNotation(aid, kind, nr, relnr, constrAid, absconstrnr, antonymic)
	}
	
	def parseLemma(n : scala.xml.Node) = {
	  val start = UtilsReader.parseSourceRef(n)
	  val sreg = Some(SourceRegion(start,start)) //no end position for lemma (proposition)
	  val l = new MizLemma(PropositionParser.parseProposition(n))
	  l.sreg = sreg
	  ParsingController.addToArticle(l)
	}
}



