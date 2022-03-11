import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.objects.Conversions.localName2OMV
import info.kwarc.mmt.api.objects.{Context, OMA, OMID, OMV, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.SyntaxPresenterServer
import info.kwarc.mmt.lf.{ApplySpine, Lambda}
import tptpexportertest.Commons._
import tptpexportertest._

import scala.collection.mutable.ListBuffer

object TPTPExporterTest extends MagicTest("debug") {

  override def doFirst(): Unit = {
    super.doFirst()
    controller.extman.addExtension(new SyntaxPresenterServer(), Nil)
    controller.handleLine("build tptpexptest/examples mmt-omdoc intro.mmt")
  }

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run(): Unit = {
    val theory = controller.getTheory(Path.parseM("https://example.com/tptpexptest?ExampleOfPLNaturalDeduction"))
    val translated = translate_theory(theory)
    println(translated)
  }

  object FOL {
    val forall: GlobalName = Path.parseS("https://example.com/tptpexptest?FOL?forall")
    val exists: GlobalName = Path.parseS("https://example.com/tptpexptest?FOL?exists")
    val ded: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?ded")
    val and: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?and")
    val or: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?or")
    val imply: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?imply")
    val equivalence: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?equivalence")
    val neg: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?neg")
  }

  abstract class TPTPDeclaration(val identifier: String)
  case class TPTPAxiom(override val identifier: String, formula: LogicFormula) extends TPTPDeclaration(identifier) {
    override def toString: String = "fof(" + identifier + ",axiom,\n    ( " + formula + " ))"
  }
  case class TPTPConjecture(override val identifier: String, formula: LogicFormula) extends TPTPDeclaration(identifier){
    override def toString: String = "fof(" + identifier + ",conjecture,\n    ( " + formula + " ))"
  }

  sealed case class TPTPTheory(name: String, axioms: List[TPTPAxiom], conjectures: List[TPTPConjecture]) {
    override def toString: String = {
      (axioms.map(_.toString) ++ conjectures.map(_.toString)).mkString("\n\n")
    }
  }
  def translate_theory(thy: Theory): TPTPTheory = {
    var axioms = new ListBuffer[TPTPAxiom]();
    var conjectures = new ListBuffer[TPTPConjecture]();
    for (decl <- thy.getDeclarations) { decl match {
      case c: Constant => {
        val translation = translate_constant(c)
        translation match {
          case Some(axiom: TPTPAxiom) => axioms += axiom
          case Some(conjecture: TPTPConjecture) => conjectures += conjecture
          case None =>
        }
      };
    }}

    TPTPTheory("", axioms.toList, conjectures.toList)
  }

  /**
    *
    * nand: prop -> prop -> prop |= [p, q] ~(p /\ q)
    * regnet: prop
    * schneit: prop
    * ax: |- nand regnet schneit
    *
    * type component of ax simplifies to `|- ~(regnet /\ schneit)`
    *
    *
    */

  def translate_constant(c: Constant)(implicit ctrl: Controller): Option[TPTPDeclaration] = {
    val ctx = Context(c.path.module)
    val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

    val newTp = c.tp.map(ctrl.simplifier(_, simplicationUnit))

    newTp match {
      case Some(ApplySpine(OMID(FOL.ded), List(formula))) =>
        Some(TPTPAxiom(c.name.toString(), translate_formula(formula)))
      //case Some(ApplySpine(OMID(FOL.ded), formula)) => //TODO: When is it a conjecture?
      //  Some(TPTPConjecture(c.name.toString(), translate_formula(formula(0))))
      case _ => None
    }
  }

  def translate_formula(t: Term): LogicFormula = t match {
    case ApplySpine(
      OMID(FOL.forall),
      List(Lambda(v, _, body))
    ) =>
      Quantified(
        !,
        Seq(
          v.toPath
        ),
        translate_formula(body)
      )
    case ApplySpine(
      OMID(FOL.exists),
      List(Lambda(v, _, body))
    ) =>
      Quantified(
        ?,
        Seq(
          v.toPath
        ),
        translate_formula(body)
      )

    case ApplySpine(OMID(FOL.and), List(left, right)) =>
      Binary(translate_formula(left), &, translate_formula(right))
    case ApplySpine(OMID(FOL.or), List(left, right)) =>
      Binary(translate_formula(left), |, translate_formula(right))
    case ApplySpine(OMID(FOL.imply), List(left, right)) =>
      Binary(translate_formula(left), Impl, translate_formula(right))
    case ApplySpine(OMID(FOL.equivalence), List(left, right)) =>
      Binary(translate_formula(left), <=>, translate_formula(right))

    case OMID(f) =>
      Atomic(Plain(Func(f.name.toString(), Nil)))
  }

  def translate_term(t: Term): Commons.Term =
    t match {
      case ApplySpine(OMID(f), args) =>
        // f: GlobalName, args: List[Term]
        Func(f.name.toString(), args.map(translate_term)) //TODO: expanding, but has to be done earlier -> Formula

      case OMID(f) =>
        Func(f.name.toString(), Nil)

      //case OMV(x) =>
      //  // x: LocalName
      //  Var(x.name.toString())
      case OMV(x) =>
        // x: LocalName
        Var(x.name.toString())
    }
}
