import info.kwarc.mmt.api.{DPath, Path}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.SyntaxPresenterServer

/**
  * Debugging links for diagram output:
  *
  * - with MMT online editor: [[http://localhost:8080/:syntax?element=latin:/algebraic/diagop-test?PlayTest]]
  *
  * - TGView2D: [[http://localhost:8080/graphs/tgview.html?type=diaggraph&graphdata=latin:/algebraic/diagop-test?PlayTest]]
  * - TGView3D: [[https://tgview3d.mathhub.info/]]
  * - JSON: [[http://localhost:8080/:jgraph/json?key=diaggraph&uri=latin:/algebraic/diagop-test?PlayTest]]
  *
  */
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
object DiagOpAlgebraTest extends MagicTest("debug") with DiagramOperatorHelper {

  override def doFirst(): Unit = {
    super.doFirst()
    controller.extman.addExtension(new SyntaxPresenterServer(), Nil)
    // hl("extension info.kwarc.mmt.api.modules.DiagramOutputServer")

    //hl("build MMT/urtheories -mmt-omdoc module-expressions.mmt")
    //hl("build MMT/urtheories mmt-omdoc module-expressions.mmt")
    //hl("build MMT/LATIN2 -mmt-omdoc algebra/diagop-test.mmt")
    hl("build MMT/LATIN2 mmt-omdoc algebra/diagop-test.mmt")
  }

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run(): Unit = {
    /*
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_Copy_Copy_Copy"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_Copy"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_CopyProjection1"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?Test_CopyProjection2"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestView_Copy"))
    waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_Copy"))*/
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExt_copy"))
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestView_copy"))
    // waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_copy"))
    //waitThenPrint(Path.parseM("http://cds.omdoc.org/urtheories?TestExtView_copy"))
    // sys.exit(0)
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

  override def run(): Unit = {
    waitThenPrint(latin ? "TestEndoMagmaSingle_pres")
    waitThenPrint(latin ? "TestEndoMagmaMulti_pres")

    waitThenPrint(latin ? "TypedUniversalQuantification_by_diagop")
  }
}

