import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.presentation.FlatMMTSyntaxPresenter
import info.kwarc.mmt.api.utils.URI

/**
  * Playground for testing out MMT/LATIN2 things
  *
  * @author Navid
  */
object Latin2Test extends MagicTest("debug"/*, "DiagramDefinition"*/) {

  final protected val latin: DPath = DPath(URI("latin:/"))

  override def doFirst: Unit = {
    // hl("file C:\\Users\\nroux\\Desktop\\kwarc-project\\code\\archives\\MathHub\\MMT\\LATIN2\\build-omdoc.msl")
    hl("build MMT/LATIN2 lf-scala")
  }

  override def run: Unit = {
  }
}