import diagops.{AdditiveOperator, PolymorphifySFOL}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ConsoleWriter
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
object DiagramPushoutTest extends MagicTest("debug") with DiagramOperatorHelper {

  override def run(): Unit = {
    val magma = Path.parseM("latin:/algebraic?Magma")
    val opposite = Path.parseM("latin:/algebraic?OppositeMagma")
    val semigroup = Path.parseM("latin:/algebraic?Semigroup")

    // TODO(NR@FR): this is necessary for the pushout operator to now throw an exception
    //              when it tries to find the assignment for [Proofs]/ded in ?OppositeMagma.
    //              Apparently, without this forced build, not all includes are visible?
    controller.handleLine("build MMT/LATIN2 mmt-omdoc domain_theories/algebra/magmas.mmt")

    val pushout = new PushoutOperator(OMMOD(opposite), magma, magma)

    val interp = new DiagramInterpreter(controller, Context.empty, new ErrorLogger(controller.report))
    pushout.applyModule(controller.getModule(semigroup))(interp)

    interp.getAddedModules.foreach(m => {
      controller.presenter.apply(m)(ConsoleWriter)
    })
  }
}

object DiagramAdditiveTest extends MagicTest("debug") with DiagramOperatorHelper {
  override def run(): Unit = {
    val semigroup = Path.parseM("latin:/algebraic?Semigroup")

    val additive = new AdditiveOperator()
    val interp = new DiagramInterpreter(controller, Context.empty, new ErrorLogger(controller.report))
    additive.applyModule(controller.getModule(semigroup))(interp)

    interp.getAddedModules.foreach(m => {
      controller.presenter.apply(m)(ConsoleWriter)
    })
  }
}

object PolymorphifyTest extends MagicTest("debug") with DiagramOperatorHelper {
  override def run(): Unit = {
    val magma = Path.parseM("latin:/algebraic?Magma")

    val op = new PolymorphifySFOL
    val interp = new DiagramInterpreter(controller, Context.empty, new ErrorLogger(controller.report))
    op.applyModule(controller.getModule(magma))(interp)

    interp.getAddedModules.foreach(m => {
      controller.presenter.apply(m)(ConsoleWriter)
    })
  }
}
