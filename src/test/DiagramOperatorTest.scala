import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.presentation.{ConsoleWriter, FlatMMTSyntaxPresenter, MMTSyntaxPresenter}
import info.kwarc.mmt.api.utils.URI

/**
  * Playground for Navid's implementation of diagram operators.
  * For debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object DiagramOperatorTest extends MagicTest("debug") {
  private var presenter: MMTSyntaxPresenter = _

  override def doFirst: Unit = {
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories mmt-omdoc")

    // Clean first preventing some spurious caching errors
    hl("build Playground/diagops -mmt-omdoc")
    hl("build Playground/diagops mmt-omdoc")

    presenter = new FlatMMTSyntaxPresenter()
    controller.extman.addExtension(presenter)
  }

  /**
    * Waits - possibly ad infinitum - for the theory given by the path to appear in the [[controller]] and present
    * it using [[presenter]].
    * @param theory A path to a theory.
    */
  private def waitAndPresent(theory: MPath): Unit = {
    var generatedTheory: Option[Theory] = None
    while (generatedTheory.isEmpty) {
      generatedTheory = controller.getO(theory).asInstanceOf[Option[Theory]]
      Thread.sleep(500)
    }
    presenter(generatedTheory.get)(ConsoleWriter)
    println("\n")
  }

  override def run : Unit = {
    val diagops = DPath(URI("https://example.com/diagops"))
    val typeindexifier = diagops / "typeindexifier"

    // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
    // hence, we apply some dirty waiting mechanism here.
    waitAndPresent(typeindexifier ? "EndoMagma_pres")
    waitAndPresent(typeindexifier ? "TypeIndexedTestTheory_pres")

    sys.exit(0)
  }
}
