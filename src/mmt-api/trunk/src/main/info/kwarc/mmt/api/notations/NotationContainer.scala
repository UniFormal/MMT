package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import collection.mutable.HashMap

class NotationDimension {
   private var _notations = new HashMap[Int,HashMap[String,TextNotation]]()
   private var _maxArity = -1 //maximum arity of this notations, smaller ones imply partial applications
   
   def notations = _notations
   
   def isDefined = !notations.isEmpty
   
   def maxArity = _maxArity
   //default notation
   def default = notations.get(maxArity).flatMap(_.get("")) //assume fully applied
   def get(arity : Int, style : String = "") : Option[TextNotation] = {
     notations.get(arity).flatMap(_.get(style))
   }
   def set(not : TextNotation, style : String = "") = {
     if (!notations.isDefinedAt(not.arity.length)) { //new arity
       notations(not.arity.length) = new HashMap[String, TextNotation]()
       if (not.arity.length > maxArity) {
         _maxArity = not.arity.length
       }
     }
     notations(not.arity.length)(style) = not
   }
   def update(nd : NotationDimension) = {
     _notations = nd.notations
     _maxArity = nd.maxArity
   }
   def delete() = notations.clear()
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
      val n1 = parsingDim.notations.values.flatten map {
         case (style,n) if ! n.isGenerated => 
           utils.xml.addAttr(utils.xml.addAttr(n.toNode, "dimensions", "1"), "style", style)
         case _ => Nil
      }
      val n2 = presentationDim.notations.values.flatten map {
         case (style, n) if ! n.isGenerated => 
           utils.xml.addAttr(utils.xml.addAttr(n.toNode, "dimensions", "2"), "style", style)
         case _ => Nil
      }
      val n3 = verbalizationDim.notations.values.flatten map {
         case (style, n) if ! n.isGenerated => 
           utils.xml.addAttr(utils.xml.addAttr(n.toNode, "dimensions", "3"), "style", style)
         case _ => Nil
      }
      if (parsingDim.isDefined || presentationDim.isDefined || verbalizationDim.isDefined)
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
         val style = utils.xml.attr(c, "style")
         utils.xml.attr(c, "dimensions") match {
            case "1" | "" => nc.parsingDim.set(tn, style)
            case "2"      => nc.presentationDim.set(tn, style)
            case "3"      => nc.verbalizationDim.set(tn, style)
         }
      }
      nc
   }
}