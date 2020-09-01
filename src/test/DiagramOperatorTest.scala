import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.presentation.{FlatMMTSyntaxPresenter, MMTSyntaxPresenter}
import info.kwarc.mmt.api.utils.URI

trait DiagramOperatorHelper {
  final protected val diagops: DPath = DPath(URI("https://example.com/diagops"))
  final protected val typeindexifier: DPath = diagops / "typeindexifier"
  final protected val typifier: DPath = diagops / "typifier"
  final protected val pushout: DPath = diagops / "pushout"
}

/**
  * Playground for Navid's implementation of diagram operators.
  * For debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object DiagramOperatorTest extends MagicTest("debug"/*, "DiagramDefinition"*/) with DiagramOperatorHelper {

  override def doFirst: Unit = {
    super.doFirst
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories -mmt-omdoc")
    // hl("build MMT/urtheories mmt-omdoc")

    // Only uncomment if rebuild is really necessary
    // hl("build MitM/Foundation mmt-omdoc")

    // Clean first to prevent some spurious caching errors
    hl("build Playground/diagops -mmt-omdoc")
    hl("build Playground/diagops mmt-omdoc")
  }

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run: Unit = {
    // Demo MultiTypeIndexifier and extension to morphisms
    waitThenPrint(typeindexifier ? "EndoMagma_pres")
    waitThenPrint(typeindexifier ? "EndoMagma_https:%2F%2Fexample.com%2Fdiagops%2Ftypeindexifier%3FOppositeMagma")
    waitThenPrint(typeindexifier ? "EndoMonoid_https:%2F%2Fexample.com%2Fdiagops%2Ftypeindexifier%3FOppositeMonoid")
    waitThenPrint(typeindexifier ? "MultiTypeIndexedTestTheory_pres")

    space()

    waitThenPrint(typeindexifier ? "EndoMagmaSingle_pres")

    waitThenPrint(typeindexifier ? "EndoMagmaSingle_pres")
    waitThenPrint(typeindexifier ? "EndoMagmaSingle_https:%2F%2Fexample.com%2Fdiagops%2Ftypeindexifier%3FOppositeMagma")
    waitThenPrint(typeindexifier ? "SingleTypeIndexedTestTheory_pres")

    space()

    waitThenPrint(typifier ? "TypifySFOLTheory_pres")

    space()

    waitThenPrint((pushout / "list") ? "ListNat_pres")
    waitThenPrint((pushout / "nvs") ? "ThingsInNormedVectorspace_pres")

    sys.exit(0)
  }
}

object DiagramClosureTest extends MagicTest("debug", "diagram", "DiagramPublisher"/*, "object-simplifier"*/) with DiagramOperatorHelper {
  override def doFirst: Unit = {
    super.doFirst
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories -mmt-omdoc")
    hl("build MMT/urtheories mmt-omdoc module-expressions.mmt")
    hl("build Playground/diagops mmt-omdoc closure/closure.mmt")
  }

  override def run: Unit = {
  }
}

object DiagramUnionTest extends MagicTest("debug", "diagram", "DiagramPublisher"/*, "object-simplifier"*/) with DiagramOperatorHelper {
  override def doFirst: Unit = {
    super.doFirst
    // Only uncomment if rebuild is really necessary
    // hl("build MMT/urtheories -mmt-omdoc")
    hl("build MMT/urtheories -mmt-omdoc module-expressions.mmt")
    hl("build MMT/urtheories mmt-omdoc module-expressions.mmt")
    hl("build Playground/diagops -mmt-omdoc union/union.mmt")
    hl("build Playground/diagops mmt-omdoc union/union.mmt")
  }

  override def run: Unit = {
  }
}

/**
  * Debugging playground for Navid's implementation of diagram operators.
  * For debugging-debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object LATIN2Test extends MagicTest("debug", "DiagramDefinition") with DiagramOperatorHelper {
  private val latin : DPath = DPath(URI("latin:/"))

  override def doFirst: Unit = {
    super.doFirst

    hl("build MMT/urtheories mmt-omdoc")
    hl("build MMT/LATIN2 scala-bin")
    hl("build MMT/LATIN2 mmt-omdoc type_theory/operators.mmt")
    hl("build MMT/LATIN2 mmt-omdoc logic/fol.mmt")
    hl("build MMT/LATIN2 mmt-omdoc logic/operators.mmt")

    // hl("build MMT/LATIN2 lf-scala")
  }

  override def run: Unit = {
    waitThenPrint(latin ? "TestEndoMagmaSingle_pres")
    waitThenPrint(latin ? "TestEndoMagmaMulti_pres")

    waitThenPrint(latin ? "TypedUniversalQuantification_by_diagop")
  }
}

