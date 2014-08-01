package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import frontend._
import checking._
import symbols._
import objects._

class RuleCache extends ChangeListener {
  override val logPrefix = "rule-cache"
     
  private var rules = new utils.HashMapToSet[MPath,Rule]

  override def onUpdate(e: ContentElement) {
     onDelete(e)
     onAdd(e)
  }
  override def onAdd(e: ContentElement) {
     e match {
        case r: RuleConstant => r.home match {
           case OMMOD(t) => rules(t) += r.df
           case _ =>
        }
        case _ =>
     }
  }
  override def onDelete(e: ContentElement) {
     e match {
        case r: RuleConstant => r.home match {
           case OMMOD(t) => rules(t) -= r.df
           case _ =>
        }
        case _ =>
     }
  }
}
