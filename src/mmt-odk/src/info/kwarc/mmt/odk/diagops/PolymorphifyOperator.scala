package info.kwarc.mmt.odk.diagops

import info.kwarc.mmt.api.modules.DiagramInterpreter
import info.kwarc.mmt.api.modules.diagops.{OperatorDSL, SimpleLinearOperator}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMS, OMV, Term}
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName, MPath, Path}
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Pi}

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

object TypifyFOLOperator extends PolymorphifyOperator {
  override val head: GlobalName = Path.parseS("latin:/fol-diagop-test?FOLDiagOps?typify_fol_operator")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sfol")

  override def operatorDomain: MPath = Path.parseM("latin:/?UntypedLogic")
  override def operatorCodomain: MPath = Path.parseM("latin:/?TypedLogic")

  override protected def indexType: Term = OMS(Path.parseS("latin:/?Types?tp"))
  override protected def baseSymbolsTranslations: Map[GlobalName, Term] = Map(
    Path.parseS("latin:/?Terms?term") -> OMS(Path.parseS("latin:/?TypedTerms?tm"))
  )
}
