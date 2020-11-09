import info.kwarc.mmt.api.{DPath, Path}
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
    hl("build MMT/urtheories mmt-omdoc module-expressions.mmt")

    // Only uncomment if rebuild is really necessary
    // hl("build MitM/Foundation mmt-omdoc")
  }

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run: Unit = {
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_Copy"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_CopyProjection1"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_CopyProjection2"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestView_Copy"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_Copy"))
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExt_copy"))
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestView_copy"))
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_copy"))
    //waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_copy"))
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

