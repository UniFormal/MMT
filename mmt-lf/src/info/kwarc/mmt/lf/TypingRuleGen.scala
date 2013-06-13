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
class TypingRuleGen extends RoleHandler with Logger {
  override val logPrefix = "rule-gen"
  def isApplicable(role: String) : Boolean = role == "Typing"
  
  case class TypingRuleGenErr(msg : String = "") extends java.lang.Throwable(msg)
    
  def apply(c : symbols.Constant) : Unit = c.tp match {
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