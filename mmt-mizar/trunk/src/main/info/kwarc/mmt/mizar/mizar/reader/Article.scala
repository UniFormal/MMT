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
		case "NotationBlock" => {
			parseNotationBlock(n)
		}
		
		case "SchemeBlock" => {
		    SchemeRegReader.parseSchemeBlock(n)
		}
		
		case "Proposition" => 
		  parseLemma(n)
		case "RegistrationBlock" => 
		  SchemeRegReader.parseRegistrationBlock(n)
		case "Set" => 
		  parseSet(n)
		case "Consider" =>
		  parseConsider(n)
		case "Reconsider" => 
		  parseReconsider(n)
		case "Proof" | "By" | "Now" |"Section" | "Reservation" => None
		case _ => {
			println("TODO: " + n.label)
		}
	}
	
	def parseSet(n : scala.xml.Node) : Unit = {
	  val nr = (n \ "@nr").text.toInt
	  val constnr = (n \ "@constnr").text.toInt
	  val tm = TypeParser.parseTerm(n.child(0))
	  val tp = TypeParser.parseTyp(n.child(1))
	  val s = new MizSet(nr, constnr, tm, tp)
	  ParsingController.addToArticle(s)
	}
	
	def parseConsider(n : scala.xml.Node) : Unit = {
	  val nr = (n \ "@nr").text.toInt
	  val constnr = (n \ "@constnr").text.toInt
	  val prop = PropositionParser.parseProposition(n.child(0))
	  val typs = n.child.slice(2, n.child.length).filter(_.label == "Typ").map(TypeParser.parseTyp).toList
	  val props = n.child.slice(2, n.child.length).filter(_.label == "Proposition").map(PropositionParser.parseProposition).toList
	  
	  val c = new MizConsider(nr, constnr, prop, typs, props)
	  ParsingController.addToArticle(c)
	}
	
	def parseReconsider(n : scala.xml.Node) : Unit = {
	  val nr = (n \ "@nr").text.toInt
	  val constnr = (n \ "@constnr").text.toInt
	  
	  val len = n.child.length
	  var terms : List[(MizTyp,MizTerm)] = Nil
	  var i : Int = 0;
	  while (i < len - 2) {
	    terms =  (TypeParser.parseTyp(n.child(i)), TypeParser.parseTerm(n.child(i+1))) :: terms
	    i += 2
	  }
	  val prop = PropositionParser.parseProposition(n.child(len - 2))
	  val r = new MizReconsider(nr, constnr, terms, prop)
	  ParsingController.addToArticle(r)
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
						case d : XMLDefinition => {
							d.setPremises(assumptions)
							d.resolveArgs()
							defs = d :: defs
						}
						case s : MizStructDef => {
						  ParsingController.addToArticle(s)//TODO
						}
						case _ => None
					}	
				}
				case _ => None 
			}
			i = i + 1
		}
		val dt = new XMLDefinitionBlock(defs)
		ParsingController.addDefinitionBlock(dt)
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
	    //val constrnr : Int = (n \ "@constrnr").text.toInt
		val d = new XMLDefTheorem(kind, nr, constrkind, 0, PropositionParser.parseProposition(n.child(0)))
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
		val constraid = (n \ "@constraid").text
		val constrkind = (n \ "@constrkind").text
		val absconstrnr = (n \ "@absconstrnr").text.toInt
		
		val pair = parseDefMeaning(n.child(n.child.length - 1))
		pair._1 	match {
			case Some(eDef) => 
				val d = new XMLIsDefiniens(aid, nr, constraid, constrkind, absconstrnr, eDef._1, eDef._2)
				ParsingController.addDefiniens(d)
			case None => pair._2 match {
				case Some(mDef) => 
				val d = new XMLMeansDefiniens(aid, nr, constraid, constrkind, absconstrnr, mDef._1, mDef._2)
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
		n.child.filter(x => x.label == "Pattern").map(parseNotationPattern)
	}
	
	def parseNotationPattern(n : Node) {
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
		val name = ParsingController.dictionary.getNameByFormatnr(kind, formatnr) match { 
			case None => 
			println("notation name not found:" + aid + kind + formatnr + constrAid)
			aid + "_" + kind + "_notation_" +  relnr 
			case Some(s) => s//TODO integrate with resolveDef from controller
		}
		//println(name)
		
		//ParsingController.dictionary.addAbsnr(kind, formatnr, nr);
		
		val not = new MizNotation(name, aid, kind, relnr, constrAid, absconstrnr, antonymic)
		ParsingController.addToArticle(not)
	}
	
	def parseLemma(n : scala.xml.Node) = {
	  val l = new MizLemma(PropositionParser.parseProposition(n))
	  ParsingController.addToArticle(l)
	}
}



