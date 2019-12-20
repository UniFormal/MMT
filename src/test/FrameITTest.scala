import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.presentation.FlatMMTSyntaxPresenter
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.frameit.FrameitServerExtension
import info.kwarc.mmt.moduleexpressions.operators.NamedPushoutUtils

/**
  * Playground for Navid's backend implementation of UFrameIT.
  * For debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object FrameITTest extends MagicTest("debug") {

  override def doFirst: Unit = {
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories -mmt-omdoc")
    // hl("build MMT/urtheories mmt-omdoc")

    // Only uncomment if rebuild is really necessary
    // hl("build MitM/Foundation mmt-omdoc")

    // Clean first to prevent some spurious caching errors
    // hl("build Playground/frameit mmt-omdoc")

    presenter = new FlatMMTSyntaxPresenter()
    controller.extman.addExtension(presenter)
    controller.extman.addExtension(new FrameitServerExtension)
  }

  final protected val frameit: DPath = DPath(URI("https://example.com/frameit"))
  final protected val annotation: DPath = frameit / "annotation"
  final protected val pushout: DPath = frameit / "pushout"

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run: Unit = {
    val thy = controller.getTheory(annotation ? "MyScrollSolution")
    val z = 80 / 10

    /*val (newTheory, newView) = NamedPushoutUtils.computeCanonicalPushoutAlongDirectInclusion(
      controller.getTheory(pushout ? "Elem"),
      controller.getTheory(pushout ? "Nat"),
      controller.getTheory(pushout ? "ListElem"),
      pushout ? "ListNat",
      controller.getAs(classOf[View], pushout ? "elemAsNat"),
      pushout ? "listElemAsListNat"
    )

    controller.add(newTheory)
    controller.add(newView)

    waitThenPrint(newTheory.path)
    waitThenPrint(newView.path)*/
  }
}
