package info.kwarc.mmt.mizar.mizar.reader

import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._


object JustificationParser {
  
  def parseJustification(n : Node) : MizJustification = n.label match {
    case "SkippedProof" => new MizSkippedProof()
    case "Proof" => parseProof(n)
    case _ => parseInference(n) 
  }
  
  def parseInference(n : Node) : MizInference = n.label match {
    case "By" => new MizBy(n.child.map(parseRef).toList)
    case "From" => new MizFrom(n.child.map(parseRef).toList)
    case "ErrorInf" => new MizErrorInf()
  }  
  
  def parseRef(n : scala.xml.Node) : MizRef = {
    val nr : Int = (n \ "@nr").text.toInt
    try {
      val kind : String = (n \ "@kind").text
      val aid : String = (n \ "@aid").text
      val absnr : Int = (n \ "@absnr").text.toInt
      new MizGlobalRef(nr, kind, aid, absnr)
    } catch {
      case _ : Throwable => new MizLocalRef(nr)
    }
  }
  
  def parseProof(n : Node) : MizProof = {
    val bt = n.child(0) //block thesis, ignored for now 
    
    val reasoning = <Reasoning>{n.child.tail}</Reasoning>
    new MizProof(parseReasoning(reasoning)) 
  }
  
  def parseReasoning(n : Node) : MizReasoning = {
    val reasoningSteps = makeAuxiliaryItems(n.child).flatMap(parseProofItem)
    new MizReasoning(reasoningSteps.toList)
  }
  
  
  def makeAuxiliaryItems(nds : Seq[Node]) : Seq[Node] = {
    var i = 0
    val n = nds.length
    var newNodes : collection.mutable.Stack[Node] = new collection.mutable.Stack[Node] 
    while (i < n) {
      val node = nds(i)
      if (node.label == "Proposition") {
        //constructing justified proposition node for more sane parsing 
        val justProp = <PropWithJust>{node}{nds(i+1)}</PropWithJust>
        i += 1
        newNodes.push(justProp)
      } else {
        newNodes.push(node)
      }
      i += 1
    }
    //TODO change Stack datatype to something else to avoid .reverse
    newNodes.toSeq.reverse
  }
  
  
  def parseProofItem(n : Node) : Option[MizProofItem] = n.label match {
    case "Let" | "Conclusion" | "Assume" | "Given" | "Take" | "TakeAsVar" => Some(parseSkeletonItem(n))
    case "PropWithJust" | "Now" | "IterEquality" | "Consider" | "Set" | "Reconsider" | "DefFunc" | "DefPred" => Some(parseAuxiliaryItem(n))
    case "PerCasesReasoning" => None //TODO
    case "EndPosition" => None
    case "Thesis" => None // comes after a skeleton item, can be ignored for now
  }
  
  
  def parseSkeletonItem(n : Node) : MizSkeletonItem = n.label match {
    case "Let" => parseLet(n)
    case "Conclusion" => parseConclusion(n)
    case "Assume" => parseAssume(n)
    case "Given" => parseGiven(n)
    case "Take" => parseTake(n)
    case "TakeAsVar" => parseTakeAsVar(n)
  }
  
  def parseLet(n : Node) : MizLet = {
    new MizLet((n \ "@nr").text.toInt, n.child.map(TypeParser.parseTyp).toList)
  }
  
  def parseConclusion(n : Node) : MizConclusion = {
    val xmljp = makeAuxiliaryItems(n.child)(0) 
    val jp = parseJustifiedProposition(xmljp) 
    new MizConclusion(jp)
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
  
  def parseAuxiliaryItem(n : Node) : MizAuxiliaryItem = n.label match {
    case "PropWithJust" | "Now" | "IterEquality" => parseJustifiedProposition(n)
    case "Consider" => parseConsider(n)
    case "Set" => parseSet(n)
    case "Reconsider" => parseReconsider(n)
    case "DefFunc" => parseDefFunc(n)
    case "DefPred" => parseDefPred(n)   
  }
  
  def parseJustifiedProposition(n : Node) : MizJustifiedProposition = n.label match {
    case "PropWithJust" => parsePropWithJust(n)
    case "Now" => parseNow(n)
    case "IterEquality" => parseIterEquality(n)
  }
  
  def parsePropWithJust(n : Node) : MizPropWithJust = {
    val prop = PropositionParser.parseProposition(n.child(0))
    val just = parseJustification(n.child(1))
    new MizPropWithJust(prop, just)
  }
  
  def parseNow(n : Node) : MizNow = {
    //dropping block thesis and computing reasoning block
    val reasoning = <Reasoning>{n.child.dropRight(1)}</Reasoning> 
    val nr = try {
      Some((n \ "@nr").text.toInt)
    } catch {
      case e : java.lang.NumberFormatException => None
    }
    val blockThesis = parseBlockThesis(n.child.last)
    new MizNow(nr, parseReasoning(reasoning), blockThesis)
  }
  
  def parseIterEquality(n : Node) : MizIterEquality = {
    val tm = TypeParser.parseTerm(n.child.head)
    val iterSteps = n.child.tail.map(parseIterStep)
    val nr = try {
      Some((n \ "@nr").text.toInt)
    } catch {
      case e : java.lang.NumberFormatException => None
    }
    new MizIterEquality(nr, tm, iterSteps.toList)     
  }
  
  def parseIterStep(n : Node) : MizIterStep = {
    val term = TypeParser.parseTerm(n.child(0))
    val inf = parseInference(n.child(1))
    new MizIterStep(term, inf)
  }
  
  def parseSet(n : Node) : MizSet = {
    val nr = (n \ "@nr").text.toInt
    val constnr = (n \ "@constnr").text.toInt
    val tm = TypeParser.parseTerm(n.child(0))
    val tp = TypeParser.parseTyp(n.child(1))
    new MizSet(nr, constnr, tm, tp)
  }
	
  def parseConsider(n : Node) : MizConsider = {
    val nr = (n \ "@nr").text.toInt
    val constnr = (n \ "@constnr").text.toInt
    val prop = PropositionParser.parseProposition(n.child(0))
    val just = parseJustification(n.child(1))
    val typs = n.child.slice(2, n.child.length).filter(_.label == "Typ").map(TypeParser.parseTyp).toList
    val props = n.child.slice(2, n.child.length).filter(_.label == "Proposition").map(PropositionParser.parseProposition).toList
	  
    new MizConsider(nr, constnr, prop, just, typs, props)
  }
	
  def parseReconsider(n : Node) : MizReconsider = {
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
    val just = parseJustification(n.child.last)
	  
    new MizReconsider(nr, constnr, terms, prop, just)
  }

  def parseDefFunc(n : Node) : MizDefFunc = {
    val nr = try {
      (n \ "@nr").text.toInt
    } catch {
      case e : Throwable => throw e
    }
    val args = n.child(0).child.map(TypeParser.parseTyp).toList
    val tm = TypeParser.parseTerm(n.child(1))
    val tp = TypeParser.parseTyp(n.child(2))
    new MizDefFunc(nr, args, tm, tp)
  }
  
  def parseDefPred(n : Node) : MizDefPred = {
    val args = n.child(0).child.map(TypeParser.parseTyp).toList
    val form = PropositionParser.parseFormula(n.child(1))
    val nr = try {
      (n \ "@nr").text.toInt
    } catch {
      case e : Throwable => throw e
    }
    new MizDefPred(nr, args, form)
  }
  
  def parseBlockThesis(n : Node) : MizBlockThesis = {
   val theses = n.child.init.map(parseThesis).toList
   val form = PropositionParser.parseFormula(n.child.last)
   new MizBlockThesis(theses, form)
  }
  
  def parseThesis(n : Node) : MizThesis = {
    val form = PropositionParser.parseFormula(n.child.head)
    new MizThesis(form)
  }
  
}