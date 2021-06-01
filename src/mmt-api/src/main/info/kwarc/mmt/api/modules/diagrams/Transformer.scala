package info.kwarc.mmt.api.modules.diagrams

/**
  * Traits for "anonymous" functorial operators, i.e., operators that are not bound
  * to an MMT symbol.
  * Anonymous operators are useful, e.g., for [[ParametricLinearOperator]]s that create them
  * on-the-fly at runtime based on the parameters they receive.
  * The main trait for linear operators is [[LinearModuleOperator]].
  *
  * @see Operators.scala for named diagram operators.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}

trait DiagramOperator {
  def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean
  def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram]

  // overwrite if necessary
  def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {}
}

trait BasedOperator {
  val dom: Diagram
  val cod: Diagram

  /**
    * @see [[applyModulePath]]
    */
  protected def applyModuleName(name: LocalName): LocalName

  /**
    * should be fast
    * pre-condition: applyModule on the module described by path returned true previously
    */
  final def applyModulePath(mpath: MPath): MPath = {
    mpath.doc ? applyModuleName(LocalName(mpath.name.head)) / mpath.name.tail
  }
}

trait Functor extends BasedOperator {
  /**
    * The functor from [[operatorDomain]] to [[operatorCodomain]], or rather,
    * its base case on individual modules.
    *
    * The actual functor is uniquely established by homomorphic extension to
    * [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms in [[applyDomain()]].
    *
    * pre-condition: there is an implicit morphism from [[operatorDomain]] to path.
    *
    * should be pretty conservative, i.e., not throw any match errors
    * if in doubt, just return ''m'' (be the "identity")
    */
  def applyDomainModule(m: MPath): MPath

  /**
    * The functor from [[operatorDomain]] to [[operatorCodomain]] uniquely described
    * by [[applyDomainModule()]].
    * @param t Any module expression over [[operatorDomain]], e.g. a composition of
    *          [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms
    */
  final def applyDomain(t: Term): Term = t match {
    case OMMOD(p) => OMMOD(applyDomainModule(p))
    case OMIDENT(t) => OMIDENT(applyDomain(t))
    case OMCOMP(mors) => OMCOMP(mors.map(applyDomain))
  }
}
