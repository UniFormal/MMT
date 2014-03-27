package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._

class NotationDimension {
   private var _notations = new collection.mutable.HashMap[String,TextNotation]()
   
   def notations = _notations
   //default notation
   def default = _notations.get("")
   def get(arity : Int, style : String = "") : Option[TextNotation] = {
     _notations.find(p => p._1 == style && p._2.arity.length == arity).map(_._2)
   }
   def set(not : TextNotation, style : String = "") = _notations(style) = not
   def update(nd : NotationDimension) = _notations = nd.notations
   def delete() = _notations.clear()
}

/** A NotationContainer wraps around various notations that can be associated with a Declaration */
class NotationContainer extends ComponentContainer {
   private val _parsingDim = new NotationDimension
   private val _presentationDim = new NotationDimension
   private val _verbalizationDim = new NotationDimension
   
   def parsingDim = _parsingDim
   def presentationDim = _presentationDim
   def verbalizationDim = _verbalizationDim
   
   def parsing = parsingDim.default
   def presentation = presentationDim.default
   def verbalization = verbalizationDim.default
   
   /*
   def parsing_=(tn: Option[TextNotation]) {
      _parsing.not = tn
   }
   def presentation_=(tn: Option[TextNotation]) {
      _presentation.not = tn
   }
   def verbalization_=(tn: Option[TextNotation]) {
      _verbalization.not = tn
   }
   */
   
   /** get the notation for a certain component */
   def apply(c: NotationComponent) = c match {
      case ParsingNotationComponent => parsing
      case PresentationNotationComponent => presentation
      case VerbalizationNotationComponent => verbalization
   }
   /** set the notation for a certain component */
   def update(c: NotationComponent, tn: TextNotation) {c match {
      case ParsingNotationComponent => parsingDim.set(tn)
      case PresentationNotationComponent => presentationDim.set(tn)
      case VerbalizationNotationComponent => verbalizationDim.set(tn)
   }}
   /** set all notations */
   def update(c: ComponentContainer) {c match {
      case nc: NotationContainer =>
         parsingDim.update(nc.parsingDim)
         presentationDim.update(nc.presentationDim)
         verbalizationDim.update(nc.verbalizationDim)
      case _ => throw ImplementationError("not a NotationContainer")
   }}
   def delete {
      parsingDim.delete
      presentationDim.delete
      verbalizationDim.delete
   }
   def isDefined = parsing.isDefined || presentation.isDefined || verbalization.isDefined
   def getComponents = parsing.toList.map(_ => (ParsingNotationComponent, this)) :::
                       presentation.toList.map(_ => (PresentationNotationComponent, this))
                       verbalization.toList.map(_ => (VerbalizationNotationComponent, this))
   
   /** @return an appropriate notation for presentation, if any */
   def getPresent: Option[TextNotation] = presentation orElse parsing orElse verbalization
   /** @return an appropriate notation for parsing, if any */
   def getParse  : Option[TextNotation] = parsing
   def getVerbal : Option[TextNotation] = verbalization orElse presentation orElse parsing
   def toNode = {
      val n1 = parsing match {
         case Some(n) if ! n.isGenerated => utils.xml.addAttr(n.toNode, "dimensions", "1")
         case _ => Nil
      }
      val n2 = presentation match {
         case Some(n) if ! n.isGenerated => utils.xml.addAttr(n.toNode, "dimensions", "2")
         case _ => Nil
      }
      val n3 = verbalization match {
         case Some(n) if ! n.isGenerated => utils.xml.addAttr(n.toNode, "dimensions", "3")
         case _ => Nil
      }
      if (parsing.isDefined || presentation.isDefined || verbalization.isDefined)
         <notation>{n1}{n2}{n3}</notation>
      else
         Nil
   }
   override def toString = {
      val s1 = parsing match {
         case Some(n) => " # " + n.toString
         case None => ""
      }
      val s2 = presentation match {
         case Some(n) => " ## " + n.toString
         case None => ""
      }
      //TODO .toString for verbalization notation
      s1 + s2
   }
}

object NotationContainer {
   def apply(): NotationContainer = new NotationContainer
   def apply(tn: TextNotation): NotationContainer = {
      val nc = apply()
      nc.parsingDim.set(tn)
      nc
   }
   def apply(tn: TextNotation, tn2: TextNotation): NotationContainer = {
      val nc = apply(tn)
      nc.presentationDim.set(tn2)
      nc
   }
   def apply(tn: TextNotation, tn2: TextNotation, tn3: TextNotation): NotationContainer = {
      val nc = apply(tn)
      nc.verbalizationDim.set(tn2)
      nc
   }
   /** parses a notation element containing several notations of different dimensions (1-dimensional is default) */
   def parse(ns: scala.xml.NodeSeq, name : GlobalName): NotationContainer = {
      val nc = new NotationContainer
      ns foreach {c =>
         val tn = TextNotation.parse(c, name)
         utils.xml.attr(c, "dimensions") match {
            case "1" | "" => nc.parsingDim.set(tn)
            case "2"      => nc.presentationDim.set(tn)
            case "3"      => nc.verbalizationDim.set(tn)
         }
      }
      nc
   }
}