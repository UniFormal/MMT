package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import collection.mutable.HashMap



class NotationDimension {
   private var _notations = new HashMap[Int,List[TextNotation]]
   private var _maxArity = -1 //maximum arity of this notations, smaller ones imply partial applications

   def notations = _notations

   def isDefined = !notations.isEmpty

   def maxArity = _maxArity
   //default notation
   def default = get(maxArity, None) //assuming fully applied

   def get(arity : Int, lang : Option[String] = None) : Option[TextNotation] = {
     notations.get(arity) match {
       case Some(l) => 
         val options = lang match {
           case None => l
           case Some(lang) => l.filter(_.scope.languages.contains(lang))
         }
         options match {
           case Nil => None 
           case _ => 
             val res = options.reduceLeft((x,y) => if (x.scope.priority > y.scope.priority) x else y)
             Some(res)
         }
       case None => None
     }
   }

   def set(not : TextNotation) = {
     val l = not.arity.length
     val nots = notations.getOrElse(l, Nil) 
     notations(l) = (not :: nots).distinct // replaces previous occurrence of same notation (which might differ in metadata)
     if (l > maxArity) {
       _maxArity = l
     }
   }
   
   def update(nd : NotationDimension) : Boolean = {
      val changed = _notations != nd.notations // over-reports changes in the rare case where nothing but the order changed
      _notations = nd.notations
      _maxArity = nd.maxArity
      changed
   }
   def delete = notations.clear
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
   
   /** get the notation for a certain component */
   def apply(c: NotationComponentKey) = c match {
      case ParsingNotationComponent => parsing
      case PresentationNotationComponent => presentation
      case VerbalizationNotationComponent => verbalization
   }
   /** set the notation for a certain component */
   def update(c: NotationComponentKey, tn: TextNotation) {c match {
      case ParsingNotationComponent => parsingDim.set(tn)
      case PresentationNotationComponent => presentationDim.set(tn)
      case VerbalizationNotationComponent => verbalizationDim.set(tn)
   }}
   /** update all notations using the values of a different container */
   def update(c: ComponentContainer) = {c match {
      case nc: NotationContainer =>
         parsingDim.update(nc.parsingDim) ||
         presentationDim.update(nc.presentationDim) ||
         verbalizationDim.update(nc.verbalizationDim)
      case _ => throw ImplementationError("not a NotationContainer")
   }}
   def delete {
      parsingDim.delete
      presentationDim.delete
      verbalizationDim.delete
   }
   def isDefined = parsing.isDefined || presentation.isDefined || verbalization.isDefined
   def getComponents = parsing.toList.map(_ => ParsingNotationComponent(this)) :::
                       presentation.toList.map(_ => PresentationNotationComponent(this))
                       verbalization.toList.map(_ => VerbalizationNotationComponent(this))
   
   /** @return an appropriate notation for presentation, if any */
   def getPresent: Option[TextNotation] = presentation orElse parsing orElse verbalization
   /** @return an appropriate notation for parsing, if any */
   def getParse  : Option[TextNotation] = parsing
   /** @return an appropriate notation for verbalization, if any */
   def getVerbal : Option[TextNotation] = verbalization
   def getAllNotations : List[TextNotation] = {
     List(parsingDim.notations.values.flatten, 
          presentationDim.notations.values.flatten,
          verbalizationDim.notations.values.flatten
     ).flatten
   }
   def toNode = {
      val n1 = parsingDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) => 
           utils.xml.addAttr(n.toNode, "dimension", "1")
         case _ => Nil
      }
      val n2 = presentationDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) => 
           utils.xml.addAttr(n.toNode, "dimension", "2")
         case _ => Nil
      }
      val n3 = verbalizationDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) => 
           utils.xml.addAttr(n.toNode, "dimension", "3")
         case _ => Nil
      }
      if (parsingDim.isDefined || presentationDim.isDefined || verbalizationDim.isDefined)
         <notations>{n1}{n2}{n3}</notations>
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
   def apply(tnOpt: Option[TextNotation]): NotationContainer = {
      val nc = apply()
      tnOpt foreach {tn => nc.parsingDim.set(tn)}
      nc
   }
   def apply(tn: TextNotation): NotationContainer = apply(Some(tn))
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
         val tn = TextNotation.parse(c, NamespaceMap(name))
         utils.xml.attr(c, "dimension") match {
            case "1" | "" => nc.parsingDim.set(tn)
            case "2"      => nc.presentationDim.set(tn)
            case "3"      => nc.verbalizationDim.set(tn)
         }
      }
      nc
   }
}