import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.presentation.{ConsoleWriter, FlatMMTSyntaxPresenter, MMTSyntaxPresenter}
import info.kwarc.mmt.api.utils.URI

/**
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

  private def getAndPresent(theory: MPath): Unit = {
    val generatedTheory = controller.getAs(classOf[Theory], theory)
    presenter(generatedTheory)(ConsoleWriter)
    println("\n")
  }

  override def run : Unit = {
    // According to Dennis, run can also be called before building ran through, which is unfortunate
    // But well, this is for debugging only anyways!

    val diagops = DPath(URI("https://example.com/diagops"))
    val typeindexifier = diagops / "typeindexifier"

    getAndPresent(typeindexifier ? "EndoMagma_pres")
    getAndPresent(typeindexifier ? "TypeIndexedTestTheory_pres")

    sys.exit(0)
  }
}
