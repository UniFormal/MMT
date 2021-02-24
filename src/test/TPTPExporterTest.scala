import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{ContainerElement, ContentElement, DPath, GlobalName, LocalName, MPath, NarrativeElement, Path}
import info.kwarc.mmt.api.objects.Conversions.localName2OMV
import info.kwarc.mmt.api.objects.{Context, IncludeVarDecl, OMA, OMATTR, OMBINDC, OMFOREIGN, OMID, OML, OMLIT, OMLITTrait, OMS, OMSemiFormal, OMV, Term, UnknownOMLIT, VarDecl}
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
    // val translated = translate_theory(theory)

    // simulate context for: [F: prop, G: prop, pf: |- F /\ G] ...[BUTTON CLICK]...
    val ctx = Context(
      IncludeVarDecl(theory.path, args = Nil),
      VarDecl(LocalName("F"), OMS(FOL.prop)),
      VarDecl(LocalName("G"), OMS(FOL.prop)),
      VarDecl(LocalName("pf"), ApplySpine(OMS(FOL.ded), ApplySpine(OMS(FOL.and), OMV("F"), OMV("G"))))
    )
    val what = ApplySpine(OMS(FOL.ded), ApplySpine(OMS(FOL.and), OMV("G"), OMV("F")))
    //print(translate_theory(theory)(controller))
    println(exportTPTP(ctx, List(what))(controller))
  }

  def exportTPTP(ctx: Context, what: List[Term])(implicit ctrl: Controller): TPTPTheory = {
    // example for context:
    //    ctx: Context = {include ?T, F: prop, G: prop, pf: |- F /\ G}
    //    (beware, pseudo notation)

    // walk through ctx, collect all axioms
    // especially, upon IncludeVarDecls, recurse into referenced theory
    val axioms = ctx.mapVarDecls {
      case (_, vd@IncludeVarDecl(_, _, _)) => {
        val path = vd.tp.get.toMPath
        translate_theory(controller.getTheory(path))
      }
      case (ctx, vd: VarDecl) => {
        translate_var_decl(vd, ctx)(ctrl).toList
      }
    }.flatten

    TPTPTheory("Theory", axioms, what.map {
      case ApplySpine(OMID(FOL.ded), List(formula)) => TPTPDeclaration("Conjecture", translate_formula(formula))
      case _ => throw new Exception("No deduction to be proved.")
    })
  }

  def translate_theory(theory: Theory)(implicit ctrl: Controller): List[TPTPDeclaration] = {
    theory.getIncludesWithoutMeta.flatMap(include => translate_theory(ctrl.getTheory(include))) ++ theory.getConstants.flatMap(translate_constant)
  }
  /**
    *
    * andI: {F: prop, G: prop} |- F -> |- G -> |- F /\ G    |# andI 1 2 3 4
    * andEL: {F: prop, G: prop} |- F /\ G -> |- F           |# andEL 1 2 3
    * andER: {F: prop, G: prop} |- F /\ G -> |- G           |# andER 1 2 3
    *
    * theory T = {
    *   ax: |- ...
    *   shorthands: ... |= ...
    *
    *   conjecture: |- F
    *
    *   conjecture': |- F -> |- G |= [f: |- F] now need to give term of type |- G
    *
    *   and_swappable: {F: prop, G: prop} |- F /\ G --> |- G /\ F
    *             |= [F: prop, G: prop, pf: |- F /\ G]
    *                    andI G F (andER F G pf) [BUTTON CLICK]
    *
    *                    (andEL F G pf) of type |- F
    * }
    *
    *    F /\ G
    * --------------   "admissible rule" (because provable from the set of postulated rules)
    *    G /\ F
    *
    *
    *  F /\ G           F /\ G
    *  -------- /\E2  --------- /\E1
    *     G               F
    *  -------------------- /\I
    *          G /\ F
    *
    */

  object FOL {
    val prop: GlobalName = Path.parseS("https://example.com/tptpexptest?FOL?prop")
    val forall: GlobalName = Path.parseS("https://example.com/tptpexptest?FOL?forall")
    val exists: GlobalName = Path.parseS("https://example.com/tptpexptest?FOL?exists")
    val ded: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?ded")
    val and: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?and")
    val or: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?or")
    val imply: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?imply")
    val equivalence: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?equivalence")
    val neg: GlobalName = Path.parseS("https://example.com/tptpexptest?PL?neg")
  }

  case class TPTPDeclaration(val identifier: String, formula: LogicFormula)
  //case class TPTPAxiom(override val identifier: String, formula: LogicFormula) extends TPTPDeclaration(identifier) {
  //  override def toString: String = "fof(" + identifier + ",axiom,\n    ( " + formula + " ))"
  //}
  //case class TPTPConjecture(override val identifier: String, formula: LogicFormula) extends TPTPDeclaration(identifier){
  //  override def toString: String = "fof(" + identifier + ",conjecture,\n    ( " + formula + " ))"
  //}

  sealed case class TPTPTheory(name: String, axioms: List[TPTPDeclaration], conjectures: List[TPTPDeclaration]) {
    override def toString: String = {
      (axioms.map(ax => "fof(" +  ax.identifier + ",axiom,\n    ( " +  ax.formula + " ))") ++ conjectures.map(cj => "fof(" +  cj.identifier + ",axiom,\n    ( " +  cj.formula + " ))")).mkString("\n\n")
    }
  }
  //def translate_theory(thy: Theory)(implicit ctrl: Controller): TPTPTheory = {
  //  var axioms = new ListBuffer[TPTPAxiom]();
  //  var conjectures = new ListBuffer[TPTPConjecture]();
  //  for (decl <- thy.getDeclarations) { decl match {
  //    case c: Constant => {
  //      val translation = translate_constant(c)
  //      translation match {
  //        case Some(axiom: TPTPAxiom) => axioms += axiom
  //        case Some(conjecture: TPTPConjecture) => conjectures += conjecture
  //        case None =>
  //      }
  //    };
  //  }}
  //
  //  TPTPTheory("", axioms.toList, conjectures.toList)
  //}

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

  def translate_var_decl(vd: VarDecl, ctx: Context)(implicit ctrl: Controller): Option[TPTPDeclaration] = {
    val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

    val newTp = vd.tp.map(ctrl.simplifier(_, simplicationUnit))

    newTp match {
      case Some(ApplySpine(OMID(FOL.ded), List(formula))) =>
        Some(TPTPDeclaration(vd.name.toString(), translate_formula(formula)))
      case _ => None
    }
  }

  def translate_constant(c: Constant)(implicit ctrl: Controller): Option[TPTPDeclaration] = {
    val ctx = Context(c.path.module)
    val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

    val newTp = c.tp.map(ctrl.simplifier(_, simplicationUnit))

    newTp match {
      case Some(ApplySpine(OMID(FOL.ded), List(formula))) =>
        Some(TPTPDeclaration(c.name.toString(), translate_formula(formula)))
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
    case ApplySpine(OMID(FOL.neg), List(arg)) =>
      Unary(Not, translate_formula(arg))

    case OMID(f) =>
      Atomic(Plain(Func(f.name.toString(), Nil)))

    case OMV(x) => Atomic(Plain(Func(x.toString, Nil)))

    case ApplySpine(OMID(f), args) => {
      Atomic(Plain(Func(f.name.toString(), args.map(translate_term))))
    }
  }

  def translate_term(t: Term): Commons.Term =
    t match {
      case ApplySpine(OMID(f), args) =>
        // f: GlobalName, args: List[Term]
        Func(f.name.toString(), args.map(translate_term))

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
