package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import frontend._
import objects._
import symbols._
import checking._
import info.kwarc.mmt.LFX.Records.{Getfield, Recexp}
import uom._
import utils._
import info.kwarc.mmt.lf._

class ImplementsRuleGenerator extends ChangeListener {
  override val logPrefix = "impl-rule-gen"
  /** the metadata key used to spot constants with implements annotations */ 
  private val implKey = Metadata.implements
  private val nameSuffix = LocalName("implements")
  
  private def getGeneratedRule(p: Path): Option[ImplementsRule] = {
     p match {
        case p: GlobalName =>
           controller.globalLookup.getO(p / nameSuffix) match {
              case Some(r: RuleConstant) => Some(r.df.asInstanceOf[ImplementsRule])
              case _ => None
           }
        case _ => None
     }
  }
  
  private def getImplemented(c: Constant): Option[GlobalName] =
    c.metadata.getLinks(implKey).headOption.map(Path.fromURI(_, NamespaceMap.empty)).flatMap {
      case n: GlobalName => Some(n)
      case _ => None
    }
     
  def onUpdate(e: StructuralElement) {
     onAdd(e)
  }
  override def onAdd(e: StructuralElement) {onCheck(e)}
  override def onDelete(e: StructuralElement) {
     getGeneratedRule(e.path).foreach {r => controller.delete(r.from.path / nameSuffix)}
  }
  override def onCheck(e: StructuralElement) {
    e match {
       case c: Constant =>
         val r = getGeneratedRule(c.path)
         if (r.isDefined) return
         val impl = getImplemented(c).getOrElse(return)
         val ruleName = c.name / nameSuffix
         log("generating rule " + ruleName)
         val rule = new ImplementsRule(c, c.home, impl)
         val ruleConst = new RuleConstant(c.home, ruleName, rule)
         ruleConst.setOrigin(GeneratedBy(this))
         controller.add(ruleConst)
       case _ =>
    }
  }
  private def error(e: StructuralElement, msg: String) {
     logError(e.path + ": " + msg)
  }
}

/**
 * a simplification rule generated by [[ImplementsRuleGenerator]]
 *
 * @param from the constant giving rise to the rule
 * @param desc string description of the rule
 * @param names structure of the left hand side
 * @param rhs the right hand side
 */
// TODO Needs to be reimplemented
class ImplementsRule(val from: Constant, recordType: Term, impl: GlobalName) extends DepthRule(impl, Recexp.path) {
    override def toString = s"$impl(record) ~~> ${from.name}(record)"
    
    def apply : Rewrite = {(bef,inn,aft) => /*
       if (aft.nonEmpty)
          NoChange
       else inn match {
          // ignoring before, which might contain, e.g., implicit arguments
          case tp :: fields if tp == recordType =>
             val t = Getfield(Recexp(fields), from.name)
             GlobalChange(t)
          case _ => NoChange
       } */ NoChange
    }

}
