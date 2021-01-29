import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.utils.URI

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
object DiagramOperatorTest extends MagicTest("debug") with DiagramOperatorHelper {

  override def run(): Unit = {
    val repl = new FastREPLExtension(FastREPL.shortcuts, Some("build MMT/urtheories mmt-omdoc module-expressions-logrel-pushout-test.mmt"))
    controller.extman.addExtension(repl)

    repl.run()
    sys.exit(0)
  }
}

/**
  * Test for ITP 2021 paper by Florian and Navid.
  */
object ITP2021Test extends MagicTest("debug") with DiagramOperatorHelper {

  override def run(): Unit = {
    val repl = new FastREPLExtension(FastREPL.shortcuts, Some("build MMT/LATIN2 mmt-omdoc itp2021/input.mmt"))
    controller.extman.addExtension(repl)

    repl.run()
    sys.exit(0)
  }
}