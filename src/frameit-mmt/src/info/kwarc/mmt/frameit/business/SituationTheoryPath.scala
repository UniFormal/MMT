package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, SimpleStep}

sealed case class SituationSpace(path: MPath) {
  def doc: DPath = path.doc
  def name: LocalName = path.name
}

sealed case class SituationTheoryPath(space: SituationSpace, name: LocalName) {
  name.steps match {
    case List(SimpleStep(_)) => // ok
    case _ => throw new Exception("complex names as situation theory names not supported currently")
  }

  def doc: DPath = space.path.doc
  def module: MPath = space.path / name
  def symbol: GlobalName = space.path ? name

  def descend(name: LocalName): SituationTheoryPath = {
    SituationTheoryPath(space, name)
  }
}

sealed class SituationTheory(val path: SituationTheoryPath)(implicit ctrl: Controller) {
  val spaceTheory: Theory = ctrl.getTheory(path.space.path)
  val theory: Theory = ctrl.getTheory(path.module)

  def space: SituationSpace = path.space

  def descend(name: LocalName): SituationTheory = {
    val newPath = path.descend(name)
    val newSituationTheory = Theory.empty(
      newPath.module.doc,
      newPath.module.name,
      theory.meta // retain meta theory
    )

    Utils.addModuleToController(newSituationTheory)
    ctrl.add(PlainInclude(path.module, newPath.module))

    new SituationTheory(newPath)
  }
}

object SituationTheory {
  def empty(doc: DPath, spaceName: LocalName, meta: Option[MPath], initialIncludes: List[MPath])(implicit ctrl: Controller): SituationTheory = {
    val spaceTheory = Theory.empty(doc, spaceName, meta)
    ctrl.add(spaceTheory)

    val rootSituationTheoryName = LocalName("Root")
    val rootSituationTheory = Theory.empty(doc, spaceTheory.name / rootSituationTheoryName, meta)
    Utils.addModuleToController(rootSituationTheory)

    initialIncludes
      .map(PlainInclude(_, rootSituationTheory.path))
      .foreach(ctrl.add(_))

    new SituationTheory(SituationTheoryPath(SituationSpace(spaceTheory.path), rootSituationTheoryName))
  }
}