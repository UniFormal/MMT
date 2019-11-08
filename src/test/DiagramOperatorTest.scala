import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.presentation.{ConsoleWriter, MMTSyntaxPresenter}
import info.kwarc.mmt.api.utils.URI

/**
  * @author Navid
  */
object DiagramOperatorTest extends MagicTest() {
  private var presenter: MMTSyntaxPresenter = _

  override def doFirst: Unit = {
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories mmt-omdoc")

    // Clean first preventing some spurious caching errors
    hl("build Playground/diagops -mmt-omdoc")
    hl("build Playground/diagops mmt-omdoc")

    presenter = new MMTSyntaxPresenter()
    controller.extman.addExtension(presenter)
  }

  override def run : Unit = {
    val generatedTheory = controller.getAs(classOf[Theory], DPath(URI("https://example.com/diagops")) ? "EndoMagma_pres")
    presenter(generatedTheory)(ConsoleWriter)

    sys.exit(0)
  }
}
