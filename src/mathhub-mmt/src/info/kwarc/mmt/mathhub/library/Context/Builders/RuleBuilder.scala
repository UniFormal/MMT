package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.symbols.RuleConstant
import info.kwarc.mmt.mathhub.library.{IDeclarationRef, IRule}

trait RuleBuilder { this: Builder =>

  /** gets a reference to a rule */
  def getRuleRef(id: String): Option[IDeclarationRef] = getReferenceOf(classOf[IDeclarationRef], id)

  /** builds a reference to a rule */
  protected def buildRuleReference(rule: RuleConstant) : Option[IDeclarationRef] = {
    val parent = getModuleRef(rule.parent.toPath)
      .getOrElse(return buildFailure(rule.parent.toPath, "getModuleRef(rule.parent)"))

    Some(
      IDeclarationRef(
        rule.path.toPath, /* id */
        rule.name.toPath, /* name */
        Some(parent), /* parent */
        "rule" /* declaration */
      )
    )
  }

  /** gets a rule */
  def getRule(id: String): Option[IRule] = getObjectOf(classOf[IRule], id)

  /** builds a rule representation */
  protected def buildRule(rule: RuleConstant) : Option[IRule] = {
    val ref = getRuleRef(rule.path.toPath)
      .getOrElse(return buildFailure(rule.path.toPath, "getRuleRef(rule.id)"))

    Some(IRule(
      ref.id, ref.name, ref.parent,
      getStats(ref.id),

      getDeclarations(rule),
      getComponents(rule),
    ))
  }
}