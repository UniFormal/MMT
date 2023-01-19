package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import notations._

/**
 * a Constant whose fields are computed on demand
 */
abstract class LazyConstant(val home : Term, val name : LocalName) extends Constant {
   protected var _alias: List[LocalName] = Nil
   protected var _tp : Option[Term] = None
   protected var _df : Option[Term] = None
   protected var _rl : Option[String] = None
   protected var _not : Option[TextNotation] = None
   protected var _vs : Option[Visibility] = None

   protected var tpDefined: Boolean = false
   protected var dfDefined: Boolean = false
   protected var otherDefined: Boolean = false

   /** called the first time the type is accessed, must set _tp */
   def onAccessTp: Unit
   /** called the first time the definiens is accessed, must set _df  */
   def onAccessDf: Unit
   /** called the first time anything else is accessed, must set _alias, _rl, and _not */
   def onAccessOther: Unit

   def tpC = {
      if (!tpDefined) {
         onAccessTp
         tpDefined = true
      }
      TermContainer(_tp)
   }
   def dfC = {
      if (!dfDefined) {
         onAccessDf
         dfDefined = true
      }
      TermContainer(_df)
   }
   def alias = {
      if (!otherDefined) {
         onAccessOther
         otherDefined = true
      }
      _alias
   }
   def rl = {
      if (!otherDefined) {
         onAccessOther
         otherDefined = true
      }
      _rl
   }
   def notC = {
      if (!otherDefined) {
         onAccessOther
         otherDefined = true
      }
      NotationContainer(_not)
   }
   def vs = {
      if (!otherDefined) {
         onAccessOther
         otherDefined = true
      }
      _vs getOrElse Visibility.public
   }
}

abstract class SimpleLazyConstant(h : Term, n : LocalName) extends LazyConstant(h,n) {
  def onAccess: Unit
  def onAccessTp: Unit = {
    onAccess
    tpDefined = true
    dfDefined = true
    otherDefined = true
  }
  def onAccessDf: Unit = {
    onAccess
    tpDefined = true
    dfDefined = true
    otherDefined = true
  }
  def onAccessOther: Unit = {
    onAccess
    tpDefined = true
    dfDefined = true
    otherDefined = true
  }
}
