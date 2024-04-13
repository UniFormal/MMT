import info.kwarc.mmt.api.modules.diagrams.{NewPushout, DiagramInterpreter}
import info.kwarc.mmt.api.objects.{Context, OMMOD}
import info.kwarc.mmt.api.presentation.{ConsoleWriter, MMTSyntaxPresenter, NotationBasedPresenter}
import info.kwarc.mmt.api.{DPath, ErrorLogger, NamespaceMap, Path, presentation}
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
    /*val x = Path.parseD("latin:/domain_theories/algebra/groups.omdoc", NamespaceMap.empty)
    val rh = new presentation.StringBuilder
    val p = new MMTSyntaxPresenter(new NotationBasedPresenter, presentGenerated = true, machineReadable = true)
    p.init(controller)
    p(controller.get(x), standalone = true)(rh)

    println(rh.get)*/

    val magma = Path.parseM("latin:/algebraic?Magma")
    val opposite = Path.parseM("latin:/algebraic?OppositeMagma")
    val semigroup = Path.parseM("latin:/algebraic?Semigroup")

    println(controller.getO(opposite))
    val pushout = new NewPushout(OMMOD(opposite), magma, magma)

    val interp = new DiagramInterpreter(controller, Context.empty, new ErrorLogger(controller.report))
    val modules = pushout.applyModule(controller.getModule(semigroup))(interp)

    modules.foreach(m => {
      controller.presenter.apply(m)(ConsoleWriter)
    })
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