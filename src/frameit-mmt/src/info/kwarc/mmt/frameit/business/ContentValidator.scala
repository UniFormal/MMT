package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler, RuleBasedChecker}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.{FinalConstant, PlainInclude}
import info.kwarc.mmt.api.{Error, ErrorContainer, MMTTask, MPath, StructuralElement}

import scala.util.Random

class ContentValidator(private val ctrl: Controller) {
  private val checker = ctrl.extman.get(classOf[MMTStructureChecker]).headOption.getOrElse({
    val checker = new MMTStructureChecker(new RuleBasedChecker)
    ctrl.extman.addExtension(checker)
      
    checker
  })

  private def createFreshCheckingEnv() = {
    val errorContainer = new ErrorContainer(None)
    val checkingEnv: CheckingEnvironment = new CheckingEnvironment (ctrl.simplifier, errorContainer, RelationHandler.ignore, MMTTask.generic)

    (checkingEnv, errorContainer)
  }
  
  private def checkStructuralElementSynchronously(element: StructuralElement): List[Error] = {
    val (checkingEnv, errorContainer) = createFreshCheckingEnv()
    checker(element)(checkingEnv) // TODO is this synchronous?

    errorContainer.getErrors
  }

  def checkTheory(theory: Theory): List[Error] = checkStructuralElementSynchronously(theory)

  def checkDeclarationAgainstTheory(theory: MPath, decl: FinalConstant): List[Error] =
    checkDeclarationAgainstTheory(ctrl.getTheory(theory), decl)

  def checkView(view: View): List[Error] = checkStructuralElementSynchronously(view)

  /**
    * Check a single declaration (that has not yet been added to `theory`) against `theory`
    * @param theory A theory
    * @param decl A "dangling" declaration, i.e. one that has not yet been added to the controller or any theory at all
    * @return A list of errors, an empty list upon success of checking
    */
  def checkDeclarationAgainstTheory(theory: Theory, decl: FinalConstant): List[Error] = {
    assert(decl.home == theory.toTerm)

    val scracthTheoryPath = theory.path ? theory.path.name.prefixOrCreateLastSimpleStep(s"scratch${Random.nextInt()}")
    val scratchTheory = Theory.empty(scracthTheoryPath.doc, scracthTheoryPath.name, theory.meta)

    ctrl.add(scratchTheory)
    ctrl.add(PlainInclude(theory.path, scratchTheory.path))

    val scratchConstant = new FinalConstant(
      OMMOD(scratchTheory.path),
      decl.name,
      decl.alias,
      decl.tpC.copy,
      decl.dfC.copy,
      decl.rl,
      decl.notC.copy,
      decl.vs
    )

    ctrl.add(scratchTheory)
    ctrl.add(scratchConstant)

    val errors = checkStructuralElementSynchronously(scratchTheory)
    ctrl.delete(scratchTheory.path)

    errors
  }
}

