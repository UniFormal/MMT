package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, SimpleStep}

sealed case class SituationSpace(path: MPath) {
  def doc: DPath = path.doc
  def name: LocalName = path.name

  override def toString: String = path.toString
}

object SituationSpace {
  /**
    * Creates a new situation space with an initial situation theory called 'name / Root'.
    * @param doc Document where to place the situation space into.
    * @param name Name of the situation space.
    * @param meta Meta theory for situation space and initial situation theory (usually containing 3D geometry)
    * @param initialIncludes Some default includes to put into the initial situation theory, e.g. inclusion
    *                        of some scroll theories.
    * @return The initial situation theory contained in the newly created situation space.
    */
  def empty(doc: DPath, name: LocalName, meta: Option[MPath], initialIncludes: List[MPath])(implicit ctrl: Controller): SituationTheory = {
    val spaceTheory = Theory.empty(doc, name, meta)
    Utils.addModule(spaceTheory)

    val rootSituationTheoryName = LocalName("Root")
    val rootSituationTheory = Theory.empty(doc, spaceTheory.name / rootSituationTheoryName, meta)
    Utils.addModule(rootSituationTheory)

    initialIncludes
      .map(PlainInclude(_, rootSituationTheory.path))
      .foreach(ctrl.add(_))

    Utils.endAddModule(spaceTheory)

    new SituationTheory(SituationTheoryPath(SituationSpace(spaceTheory.path), rootSituationTheoryName))
  }
}

sealed case class SituationTheoryPath(space: SituationSpace, name: LocalName) {
  name.steps match {
    case List(SimpleStep(_)) => // ok
    case _ => throw new Exception("complex names as situation theory names not supported currently")
  }

  def doc: DPath = space.path.doc
  def module: MPath = space.path / name
  def symbol: GlobalName = space.path ? name

  override def toString: String = s"situation theory `${name}` in space ${space}"

  def descend(name: LocalName): SituationTheoryPath = {
    SituationTheoryPath(space, name)
  }
}

sealed class SituationTheory(val path: SituationTheoryPath)(implicit ctrl: Controller) {
  val spaceTheory: Theory = ctrl.getTheory(path.space.path)
  val theory: Theory = ctrl.getTheory(path.module)

  def space: SituationSpace = path.space

  override def toString: String = path.toString

  def descend(name: LocalName): SituationTheory = {
    val newPath = path.descend(name)
    val newSituationTheory = Theory.empty(
      newPath.module.doc,
      newPath.module.name,
      theory.meta // retain meta theory
    )

    Utils.addModule(newSituationTheory)
    val include = PlainInclude(path.module, newPath.module)
    ctrl.add(include)
    ctrl.endAdd(include)
    Utils.endAddModule(newSituationTheory)

    new SituationTheory(newPath)
  }
}
