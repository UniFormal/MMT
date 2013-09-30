/**
 *
 */
package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._
/**
 * @author aivaras
 *
 */
/*
class TypingRuleGen extends ChangeListener {
  override val logPrefix = "rule-gen"
  case class TypingRuleGenErr(msg : String = "") extends java.lang.Throwable(msg)
    
  def onCheck(ce : ContentElement) {c.tp match {
    case Some(tm) => {
      val ruleName : String = c.name.toString
      tm match {
        case FunType(guards, judg) =>
        case _ =>
      }
      Unit
    }
    case _ => throw TypingRuleGenErr("no term in " ++ c.name.toString)
  }
}

*/