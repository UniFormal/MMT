package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.checking.{CheckingEnvironment, CheckingUnit, MMTStructureChecker, RelationHandler, RuleBasedChecker, StructureChecker}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.{FinalConstant, PlainInclude}
import info.kwarc.mmt.api.{CPath, ComplexStep, DefComponent, Error, ErrorContainer, GeneralError, GetError, GlobalName, InvalidElement, InvalidUnit, LocalName, LookupError, MMTTask, MPath, StructuralElement}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SCheckingError, SInvalidScrollAssignment, SMiscellaneousError, SNonTotalScrollApplication, SScrollAssignments}

import scala.util.Random

trait ContentValidator {
  def checkTheory(theory: Theory): List[Error]

  def checkView(view: View): List[Error]

  def checkScrollView(view: View, originalAssignments: SScrollAssignments): List[SCheckingError]

  def checkDeclarationAgainstTheory(theory: Theory, decl: FinalConstant): List[Error]
}

class NopContentValidator extends ContentValidator {
  override def checkTheory(theory: Theory): List[Error] = Nil

  override def checkView(view: View): List[Error] = Nil

  override def checkScrollView(view: View, originalAssignments: SScrollAssignments): List[SCheckingError] = Nil

  override def checkDeclarationAgainstTheory(theory: Theory, decl: FinalConstant): List[Error] = Nil
}

class StandardContentValidator(implicit ctrl: Controller) extends ContentValidator {
  private val checker: StructureChecker = ctrl.extman.get(classOf[MMTStructureChecker]).headOption.getOrElse({
    val checker = new MMTStructureChecker(new RuleBasedChecker)
    ctrl.extman.addExtension(checker)
      
    checker
  })

  private def createFreshCheckingEnv() = {
    val errorContainer = new ErrorContainer(None)
    val checkingEnv: CheckingEnvironment = new CheckingEnvironment(ctrl.simplifier, errorContainer, RelationHandler.ignore, MMTTask.generic)

    (checkingEnv, errorContainer)
  }
  
  private def checkStructuralElementSynchronously(element: StructuralElement): List[Error] = {
    val (checkingEnv, errorContainer) = createFreshCheckingEnv()

    try {
      // TODO use checker.applyWithTimeout lest the server get stuck
      checker(element)(checkingEnv) // TODO is this synchronous?
    } catch {
      case err @ (_: LookupError | _: GetError) =>
        errorContainer(err.asInstanceOf[Error]) // Scala requires this unnecessary casting for some reason
    }

    errorContainer.getErrors
  }

  override def checkTheory(theory: Theory): List[Error] = checkStructuralElementSynchronously(theory)

  override def checkView(view: View): List[Error] = checkStructuralElementSynchronously(view)

  override def checkScrollView(view: View, originalAssignments: SScrollAssignments): List[SCheckingError] = {
    val viewPath = view.path

    checkView(view).map {
      // typechecking errors of view assignments
      case err @ InvalidUnit(CheckingUnit(
      // due to MMT's generality, these are reported to happen in the view's assignments-as-constants'
      // definition components
      Some(CPath(GlobalName(`viewPath`, LocalName(ComplexStep(thy) :: symbol)), DefComponent)),
      _, _, _
      ), _, _) =>
        // double-check that the assignment originated from us
        originalAssignments.assignments.find(_._1.uri == thy ? symbol) match {
          case Some((factRef, _)) => SInvalidScrollAssignment(err.msg, factRef)

          case None => SMiscellaneousError(err.msg)
        }

      case GetError(msg) if msg.contains("no assignment for") =>
        SNonTotalScrollApplication()

      case InvalidElement(`view`, msg) if msg.contains("not total") =>
        SNonTotalScrollApplication()
      case err => SMiscellaneousError(err.getMessage)
    }.distinct
  } // clear possibly duplicate [[SNonTotalScrollApplication()]] objects

  /**
    * Check a single declaration (that has not yet been added to `theory`) against `theory`
    * @param theory A theory
    * @param decl A "dangling" declaration, i.e. one that has not yet been added to the controller or any theory at all
    * @return A list of errors, an empty list upon success of checking
    */
  override def checkDeclarationAgainstTheory(theory: Theory, decl: FinalConstant): List[Error] = {
    assert(decl.home == theory.toTerm)

    val scratchTheoryPath = theory.path ? theory.path.name.prefixOrCreateLastSimpleStep(s"scratch${Random.nextInt()}")
    val scratchTheory = Theory.empty(scratchTheoryPath.doc, scratchTheoryPath.name, theory.meta)

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

