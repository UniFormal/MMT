package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import frontend._
import checking._
import symbols._
import objects._

// unused
class RuleCache extends ChangeListener {
  override val logPrefix = "rule-cache"

  private val rules = new utils.HashMapToSet[MPath,Rule]

  override def onAdd(e: StructuralElement) {
     e match {
        case r: RuleConstant => r.home match {
           case OMMOD(t) => r.df foreach {rdf => rules(t) += rdf}
           case _ =>
        }
        case _ =>
     }
  }
  override def onDelete(e: StructuralElement) {
     e match {
        case r: RuleConstant => r.home match {
           case OMMOD(t) => r.df foreach {rdf => rules(t) -= rdf}
           case _ =>
        }
        case _ =>
     }
  }
}
