package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import parser.TextNotation

class NotationDimension {
   var not : Option[TextNotation] = None
}

/** A NotationContainer wraps around various notations that can be associated with a Symbol */
class NotationContainer {
   private val _oneDim = new NotationDimension
   private val _twoDim = new NotationDimension
   
   def oneDim = _oneDim.not
   def twoDim = _twoDim.not
   def oneDim_=(tn: TextNotation) {
      _oneDim.not = Some(tn)
   }
   def twoDim_=(tn: TextNotation) {
      _twoDim.not = Some(tn)
   }
   
   /** get the notation for a certain component */
   def apply(c: NotationComponent) = c match {
      case OneDimNotationComponent => oneDim
      case TwoDimNotationComponent => twoDim
   }
   /** set the notation for a certain component */
   def update(c: NotationComponent, tn: TextNotation) {c match {
      case OneDimNotationComponent => oneDim = tn
      case TwoDimNotationComponent => twoDim = tn
   }}
   
   /** @return an appropriate notation for presentation, if any */
   def getPresent: Option[TextNotation] = twoDim orElse oneDim
   /** @return an appropriate notation for parsing, if any */
   def getParse  : Option[TextNotation] = oneDim
   def toNode = {
      val n1 = oneDim match {
         case Some(n) => utils.xml.addAttr(n.toNode, "dimensions", "1")
         case None => Nil
      }
      val n2 = twoDim match {
         case Some(n) => utils.xml.addAttr(n.toNode, "dimensions", "2")
         case None => Nil
      }
      if (oneDim.isDefined || twoDim.isDefined)
         <notation>{n1}{n2}</notation>
      else
         Nil
   }
   override def toString = {
      val s1 = oneDim match {
         case Some(n) => " # " + n.toString
         case None => ""
      }
      val s2 = twoDim match {
         case Some(n) => " ## " + n.toString
         case None => ""
      }
      s1 + s2
   }
}

object NotationContainer {
   def apply(): NotationContainer = new NotationContainer
   def apply(tn: TextNotation): NotationContainer = {
      val nc = apply()
      nc.oneDim = tn
      nc
   }
   def apply(tn: TextNotation, tn2: TextNotation): NotationContainer = {
      val nc = apply(tn)
      nc.twoDim = tn2
      nc
   }
   /** parses a notation element containing several notations of different dimensions (1-dimensional is default) */
   def parse(ns: scala.xml.NodeSeq, name : GlobalName): NotationContainer = {
      val nc = new NotationContainer
      ns foreach {c =>
         val tn = TextNotation.parse(c, name)
         utils.xml.attr(c, "dimensions") match {
            case "1" | "" => nc.oneDim = tn
            case "2"      => nc.twoDim = tn
         }
      }
      nc
   }
}