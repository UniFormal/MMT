package info.kwarc.mmt.odk.diagops

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.diagrams.{Diagram, DiagramInterpreter, OperatorDSL, SimpleLinearOperator}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api.uom.TheoryScala
import info.kwarc.mmt.lf._

abstract class PolymorphifyOperator extends SimpleLinearOperator with OperatorDSL {
  protected def indexType: Term
  protected def baseSymbolsTranslations: Map[GlobalName, Term]

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {

    val seenPaths = state.seenDeclarations.map(_.path).toSet

    val newTp = c.tp.map(tp => Pi(LocalName("u"), indexType, index(tp, OMV("u"), seenPaths)))
    val newDf = c.df.map(df => Lambda(LocalName("u"), indexType, index(df, OMV("u"), seenPaths)))
    val newNotation = c.notC.copy().mapInPlace(not => {
      try {
        not.copy(fixity = not.fixity.addInitialImplicits(1))
      } catch {
        case _: ImplementationError => not // TODO: until Florian has implemented addInitialImplicits for all marker types
      }
    })

    List(Constant(
      home = state.outContainer.toTerm,
      name = emptyRenamer(c.name),
      alias = c.alias,
      tp = newTp,
      df = newDf,
      rl = c.rl,
      not = newNotation
    ))
  }

  private def index(t: Term, indexVariable: OMV, seenPaths: Set[GlobalName]): Term = {
    new OMSReplacer {
      override def replace(p: GlobalName): Option[Term] = {
        if (seenPaths.contains(p)) {
          Some(ApplySpine(OMS(applyModulePath(p.module) ? p.name), indexVariable))
        } else if (baseSymbolsTranslations.contains(p)) {
          Some(ApplySpine(baseSymbolsTranslations(p), indexVariable))
        } else {
          None
        }
      }
    }.apply(t, Context.empty)
  }
}

// TODO: This should go to LATIN2
object PolymorphifyOperator_ITP2021 extends PolymorphifyOperator {
  override val head: GlobalName = Path.parseS("latin:/casestudies/itp2021?DiagOpsMeta?polymorphify_operator")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_poly")

  private object Untyped extends TheoryScala {
    override val _base: DPath = Path.parseD("latin:/casestudies/itp2021", NamespaceMap.empty)
    override val _name: LocalName = LocalName("UTyped")
    object term extends NullaryLFConstantScala(_path, "term")
  }

  private object HardTyped extends TheoryScala {
    override val _base: DPath = Path.parseD("latin:/casestudies/itp2021", NamespaceMap.empty)
    override val _name: LocalName = LocalName("HTyped")
    object tp extends NullaryLFConstantScala(_base ? "Types", "tp")
    object tm extends UnaryLFConstantScala(_base ? "Terms", "tm")
  }

  override def operatorDomain: Diagram = Diagram.singleton(Untyped._path)
  override def operatorCodomain: Diagram = Diagram.singleton(HardTyped._path)

  override protected def indexType: Term = HardTyped.tp.term
  override protected def baseSymbolsTranslations: Map[GlobalName, Term] = Map(
    Untyped.term.path -> HardTyped.tm.term
  )
}

object TypifyFOLOperator extends PolymorphifyOperator {
  override val head: GlobalName = Path.parseS("latin:/fol-diagop-test?FOLDiagOps?typify_fol_operator")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sfol")

  override def operatorDomain: Diagram = Diagram(List(
    Path.parseM("latin:/?PLND"),
    Path.parseM("latin:/?UntypedLogic"),
    Path.parseM("latin:/?Terms"),

    Path.parseM("latin:/?OneTyped"), Path.parseM("latin:/?Relation"), Path.parseM("latin:/?Reflexivity"), Path.parseM("latin:/?Symmetry"), Path.parseM("latin:/?Transitivity"), Path.parseM("latin:/?Preorder"), Path.parseM("latin:/?EquivalenceRelation"), Path.parseM("latin:/?Congruence"), Path.parseM("latin:/?EquivalenceCongruence")
  ))
  override def operatorCodomain: Diagram = Diagram.singleton(Path.parseM("latin:/?TypedLogic"))

  override def applyMetaModule(t: Term)(implicit lookup: Lookup): Term = t match {
    case OMMOD(p) if p == Path.parseM("latin:/?UntypedLogic") => OMMOD(Path.parseM("latin:/?TypedLogic"))
    case t => t
  }

  override protected def indexType: Term = OMS(Path.parseS("latin:/?Types?tp"))
  override protected def baseSymbolsTranslations: Map[GlobalName, Term] = Map(
    Path.parseS("latin:/?Terms?term") -> OMS(Path.parseS("latin:/?TypedTerms?tm"))
  )
}
