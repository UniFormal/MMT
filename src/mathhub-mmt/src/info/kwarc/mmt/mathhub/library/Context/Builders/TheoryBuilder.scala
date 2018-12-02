package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.modules.{DeclaredTheory, Theory}
import info.kwarc.mmt.mathhub.library.{ITheory, IModuleRef}

trait TheoryBuilder { this: Builder =>

  /** gets a reference to a theory */
  def getTheoryRef(id: String): Option[IModuleRef] = getReferenceOf(classOf[IModuleRef], id)

  /** builds a reference to a theory */
  protected def buildTheoryReference(theory: Theory) : Option[IModuleRef] = Some(
    IModuleRef(
      theory.path.toPath, /* id */
      theory.name.toPath, /* name */
    )
  )

  /** gets a theory */
  def getTheory(id: String): Option[ITheory] = getObjectOf(classOf[ITheory], id)

  /** builds a theory representation */
  protected def buildTheory(theory: Theory): Option[ITheory] = {
    val ref = getTheoryRef(theory.path.toPath)
      .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(theory.id)"))

    val meta = theory match {
      case dc: DeclaredTheory =>
        dc.meta.map(mt =>
          getTheoryRef(mt.toPath)
            .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(theory.meta)")
            )
        )
      case _ => None
    }

    Some(ITheory(
      ref.id, ref.name,
      getStats(ref.id),

      getDeclarations(theory),

      meta
    ))
  }
}