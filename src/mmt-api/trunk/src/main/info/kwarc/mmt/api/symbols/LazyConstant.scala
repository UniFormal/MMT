package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import notations._

/**
 * a Constant whose fields are computed on demand
 */
abstract class LazyConstant(val home : Term, val name : LocalName) extends Constant {
   protected var _alias: Option[LocalName] = None
   protected var _tp : Option[Term] = None
   protected var _df : Option[Term] = None
   protected var _rl : Option[String] = None
   protected var _not : Option[TextNotation] = None
   
   private var tpAccessed: Boolean = false
   private var dfAccessed: Boolean = false
   private var otherAccessed: Boolean = false

   /** called the first time the type is accessed, must set _tp */
   def onAccessTp
   /** called the first time the definiens is accessed, must set _df  */
   def onAccessDf
   /** called the first time anything else is accessed, must set _alias, _rl, and _not */
   def onAccessOther

   def tpC = {
      if (tpAccessed)
         onAccessTp
      TermContainer(_tp)
   }
   def dfC = {
      if (dfAccessed)
         onAccessDf
      TermContainer(_df)
   }
   def alias = {
      if (otherAccessed)
         onAccessOther
      _alias
   }
   def rl = {
      if (otherAccessed)
         onAccessOther
      _rl
   }
   def notC = {
      if (otherAccessed)
         onAccessOther
      NotationContainer(_not)
   }
}