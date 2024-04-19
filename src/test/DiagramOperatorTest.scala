import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.modules.diagrams.{Diagram, DiagramConnection, DiagramFunctor, DiagramInterpreter, LinearOperators, NewPushout}
import info.kwarc.mmt.api.notations.{Delim, Delimiter, Mixfix, NotationContainer, SimpArg, TextNotation}
import info.kwarc.mmt.api.objects.{Context, OMID, OMIDENT, OMMOD, StatelessTraverser, Term, Traverser, UniformTranslator}
import info.kwarc.mmt.api.presentation.{ConsoleWriter, MMTSyntaxPresenter, NotationBasedPresenter}
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}
import info.kwarc.mmt.api.{ComplexStep, DPath, ErrorLogger, GlobalName, LocalName, MPath, NamespaceMap, Path, presentation}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Apply

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

    println(controller.getO(opposite))
    val pushout = new NewPushout(OMMOD(opposite), magma, magma)

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

    val additive = new Additive()
    val interp = new DiagramInterpreter(controller, Context.empty, new ErrorLogger(controller.report))
    additive.applyModule(controller.getModule(semigroup))(interp)

    interp.getAddedModules.foreach(m => {
      controller.presenter.apply(m)(ConsoleWriter)
    })
  }
}

class Additive extends LinearOperators {
  private val Magma: MPath = Path.parseM("latin:/algebraic?Magma")
  private val Set: MPath = Path.parseM("latin:/algebraic?Set")

  private object Symbols {
    val binaryOp: GlobalName = Magma ? "op"
    val inverseOp: GlobalName = Path.parseM("latin:/algebraic?InverseOperator") ? "inv"
    val unit: GlobalName = Path.parseM("latin:/algebraic?UnitElement") ? "unit"
  }

  override val ops: LinearOperatorSpecs = resolve(List(
    ("A", Diagram.singleton(Set) -> Diagram.singleton(Set), DiagramFunctor.singleton(Set, Set), prefixBy("Additive")),
    ("inA", Id(Set) -> "A", DiagramConnection.Singleton(Set, Set, OMIDENT(OMMOD(Set))), suffixBy("AsAdditive")),
    ("outA", "A" -> Id(Set), DiagramConnection.Singleton(Set, Set, OMIDENT(OMMOD(Set))), suffixBy("AsMultiplicative"))
  ))

  private val equinamer = getEquinamer("A")

  override def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {

    val modifications: Map[GlobalName, () => (TextNotation, List[LocalName])] = Map(
      Symbols.binaryOp -> (() => (
        c.not.get.copy(fixity = Mixfix(List(SimpArg(1), Delim("+"), SimpArg(2)))),
        List(LocalName("plus"))
      )),
      Symbols.inverseOp -> (() => (
        c.not.get.copy(fixity = Mixfix(List(Delim("-", associatesToLeft = false), SimpArg(1)))),
        List(LocalName("negate"))
      )),
      Symbols.unit -> (() => (
        c.not.get.copy(fixity = Mixfix(List(Delim("0")))),
        List(LocalName("zero"))
      ))
    )

    def inTr(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, OMMOD(ops("inA")(c.parent)))

    val newC = new FinalConstant(
      home = OMMOD(ops("A")(c.parent)),
      name = equinamer(c.name),
      alias = c.alias.map(n => equinamer(n)) ++ modifications.get(c.path).map(f => f()._2).getOrElse(Nil),
      tpC = c.tpC map inTr,
      dfC = c.dfC map inTr,
      rl = c.rl,
      notC = modifications.get(c.path).map(f => NotationContainer(f()._1)).getOrElse(c.notC),
      vs = c.vs
    )
    interp.add(newC)

    if (c.df.isEmpty) {
      val inC = Constant(
        home = OMMOD(ops("inA")(c.parent)),
        name = LocalName(ComplexStep(c.parent) :: c.name),
        alias = Nil,
        tp = c.tpC.map(inTr).get,
        df = Some(newC.toTerm),
        rl = None
      )
      val outC = Constant(
        home = OMMOD(ops("outA")(c.parent)),
        name = LocalName(ComplexStep(newC.parent) :: equinamer(c.name)),
        alias = Nil,
        tp = c.tpC.get,
        df = Some(c.toTerm),
        rl = None
      )
      interp.add(inC)
      interp.add(outC)
    }
  }
}

class FOL2SFOL extends LinearOperators {
  private val FOL: MPath = Path.parseM("latin:/?FOL")
  private val SFOL: MPath = Path.parseM("latin:/?SingleUniverseSFOL")

  private val term: GlobalName = ???
  private val tm: GlobalName = ???
  private val designatedTp: GlobalName = SFOL ? "U"

  override val ops: LinearOperatorSpecs = resolve(List(
    ("F", Diagram.singleton(FOL) -> Diagram.singleton(SFOL), DiagramFunctor.singleton(FOL, SFOL), suffixBy("_SFOL"))
  ))

  private val equinamer = getEquinamer("P")

  override def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {
    val trr = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMID(`term`) => Apply(OMID(tm), OMID(designatedTp))
        case t => Traverser.apply(this, t)
      }
    }

    val tr = new UniformTranslator {
      override def applyPlain(con: Context, tm: Term): Term = trr(tm, con)
    }

    val newC = new FinalConstant(
      home = OMMOD(ops("F")(c.parent)),
      name = equinamer(c.name),
      alias = c.alias.map(n => equinamer(n)),
      tpC = c.tpC.map(t => tr.applyType(Context(c.parent), t)),
      dfC = c.dfC.map(t => tr.applyDef(Context(c.parent), t)),
      rl = c.rl,
      notC = NotationContainer.empty(),
      vs = c.vs
    )
    newC.metadata.add(c.metadata.getAll : _*)

    interp.add(newC)
  }
}
