package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.modules.{DeclaredTheory, Theory}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{ITheory, ITheoryRef}

trait TheoryBuilder { this: Builder =>

  /** gets a reference to a theory */
  def getTheoryRef(id: String): Option[ITheoryRef] = getReferenceOf(classOf[ITheoryRef], id)

  /** builds a reference to a theory */
  protected def buildTheoryReference(theory: Theory) : Option[ITheoryRef] = Some(
    ITheoryRef(
      theory.path.toPath, /* id */
      theory.name.toPath /* name */
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

    val presentation: String = getPresentationOf(theory)
    val source: Option[String] = getSourceOf(theory)

    Some(ITheory(
      ref.id, ref.name,
      getStats(ref.id),

      presentation,
      source,

      meta
    ))
  }
}