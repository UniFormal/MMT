package info.kwarc.mmt.api.modules.diagrams

/**
  * Traits for "anonymous" functorial operators, i.e., operators that are not bound
  * to an MMT symbol.
  * Anonymous operators are useful, e.g., for [[ParametricLinearOperator]]s that create them
  * on-the-fly at runtime based on the parameters they receive.
  * The main trait for linear operators is [[LinearModuleTransformer]].
  *
  * @see Operators.scala for named diagram operators.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  // diagram X := FORCE_BIND (HOM ourDiagram) /algebra
  // FORCE_BIND: DiagramOperator -> Namespace -> DiagramOperator
  // FORCE_BIND(op, n) := new DiagramOperator {
  //   def applyModulePath(mpath: MPath) = n ? op.applyModulePath(mpath)
  //   // otherwise, delegate to op
  // }

  /**
    * For [[LinearModuleTransformer]]s: only call this function IF YOU KNOW that the operator transforms mpath
    *   e.g. if mpath stems from operator domain and is left untouched, DO NOT CALL THIS FUNCTION
    */
  final def applyModulePath(mpath: MPath): MPath = {
    mpath.doc ? applyModuleName(LocalName(mpath.name.head)) / mpath.name.tail
    /*if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }*/
  }
}

trait DiagramTransformer {
  def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean
  def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram]

  // overwrite if necessary
  def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {}
}

trait BaseTransformer {
  def operatorDomain: Diagram
  def operatorCodomain: Diagram

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

trait FunctorTransformer extends BaseTransformer {
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

trait NatTransTransformer extends BaseTransformer {
  /**
    * must output a morphism term
    * @param thy
    *
    * should be pretty conservative, i.e., not throw any match errors
    * if in doubt, just return ''OMMOD(thy)'' (be the "identity")
    *
    * @return
    */
  def applyDomainTheory(thy: MPath): Term

  /**
    * pre-condition: only applicable on theory expressions
    * @param t
    * @return
    */
  final def applyDomain(t: Term): Term = t match {
    case OMMOD(p) /* ideally: only if p points to a theory */ => applyDomainTheory(p)
  }
}
