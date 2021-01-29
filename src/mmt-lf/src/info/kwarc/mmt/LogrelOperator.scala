package info.kwarc.mmt

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, Include, PlainInclude}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.lf.LF

/**
  * The type of a logical relation; do not confuse "type" with MMT types.
  *
  * Currently, the morphisms' domain and codomain are limitied to theories, i.e. [[MPath]]s
  * due to limitiations in the diagram operator framework, e.g. domain and codomain must be
  * usable as elements in a [[Diagram]].
  */
sealed case class LogrelType(mors: List[Term], commonMorDomain: MPath, commonMorCodomain: MPath)

/**
  * References a concrete logical relation formalized in OMDoc
  * @param logrelType The type of it
  * @param logrel The realization of the logrel interface theory (i.e. an MMT morphism) represented as
  *               a [[Term]]
  */
sealed case class ConcreteLogrel(logrelType: LogrelType, logrel: Term)

final class LogrelTransformer(
                               logrelType: MPath => LogrelType,
                               baseLogrelInfo: Option[ConcreteLogrel] = None
                             ) extends SimpleLinearModuleTransformer with OperatorDSL {

  override val operatorDomain: Diagram   =
    Diagram(List(LF.theoryPath) ::: baseLogrelInfo.map(_.logrelType.commonMorDomain).toList)
  override val operatorCodomain: Diagram =
    Diagram(List(LF.theoryPath) ::: baseLogrelInfo.map(_.logrelType.commonMorCodomain).toList)

  override def applyMetaModule(t: Term)(implicit lookup: Lookup): Term = t match {
    case OMMOD(p) if baseLogrelInfo.exists(_.logrelType.commonMorDomain == p) =>
      OMMOD(baseLogrelInfo.get.logrelType.commonMorCodomain)

    case t => t
  }

  // todo: encode links in name?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel")

  override protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    val currentLinkDomain = logrelType(thy.path).commonMorDomain
    /*if (!interp.ctrl.globalLookup.hasImplicit(thy.path, currentLinkDomain)) {
      interp.errorCont(InvalidElement(thy, s"Failed to apply logrel operator on `${thy.path}` because that theory " +
        s"is outside the domain of the morphisms, namely `$currentLinkDomain`."))
      return None
    }*/

    super.beginTheory(thy, state).map(outTheory => {
      val include = PlainInclude(logrelType(thy.path).commonMorCodomain, outTheory.path)
      interp.add(include)
      interp.endAdd(include)

      outTheory
    })
  }

  override protected def beginView(view: View, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    super.beginView(view, state).map(outView => {
      val currentLogrelCodomain = logrelType(view.to.toMPath).commonMorCodomain
      val include = Include.assignment(
        outView.toTerm,
        currentLogrelCodomain,
        Some(OMIDENT(OMMOD(currentLogrelCodomain)))
      )
      interp.add(include)
      interp.endAdd(include)

      outView
    })
  }

  val logrelRenamer: Renamer[LinearState] = getRenamerFor("_r") // getRenamerFor("ʳ")

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val logrel: PartialLogrel = {
      val currentLogrelType = state.inContainer match {
        case thy: Theory => logrelType(thy.path)
        case link: Link => logrelType(link.to.toMPath)
      }

      val logrelBase: GlobalName => Option[Term] = p => {
        if ((state.processedDeclarations ++ state.skippedDeclarations).exists(_.path == p)) {
          Some(OMS(logrelRenamer(p)))
        } else baseLogrelInfo match {
          case Some(ConcreteLogrel(baseLogrelType, baseLogrel))
            => // if interp.ctrl.globalLookup.hasImplicit(p.module, baseLogrelType.commonMorDomain) =>

            try {
              Some(interp.ctrl.globalLookup.ApplyMorphs(
                OMS(logrelRenamer.applyAlways(p)),
                baseLogrel
              ))
            } catch {
              case err: GetError =>
                println(err) // todo: do not use exceptions for control flow here! println is a reminder to redo it
                None
            }

          case _ =>
            return NotApplicable(c, "refers to constant not previously seen. Implementation error?")
        }
      }

      new PartialLogrel(currentLogrelType.mors, logrelBase, interp.ctrl.globalLookup)
    }

    def betaReduce(t: Term): Term = {
      val su = SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true)
      interp.ctrl.simplifier(t, su, RuleSet(lf.Beta))
    }

    val newTp = c.tp.flatMap(logrel.getExpected(Context.empty, c.toTerm, _)).map(betaReduce)
    val newDf = c.df.flatMap(logrel.apply(Context.empty, _)).map(betaReduce)

    newTp match {
      case Some(_) => List(Constant(
        home = state.outContainer.toTerm,
        name = logrelRenamer(c.name),
        alias = Nil,
        tp = newTp,
        df = newDf,
        rl = None
      ))

      case None => Nil
    }
  }
}

abstract class LogrelOperator extends ParametricLinearOperator {
  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = {
    parameters match {
      case mors if mors.nonEmpty =>
        LogrelOperator.parseLogRelInfo(mors)(interp.ctrl.globalLookup).map(logrelType =>
          new LogrelTransformer((_: MPath) => logrelType)
        )

      case _ =>
        None
    }
  }
}

object LogrelOperator {
  def parseLogRelInfo(mors: List[Term])(implicit lookup: Lookup): Option[LogrelType] = {
    val (commonMorDomain, commonMorCodomain) =
      mors.map(mor => (Morph.domain(mor), Morph.codomain(mor))).distinct match {
        // TODO: support complex morphisms
        //       problem so far: MPaths required to define operatorDomain way above in this file
        case List((Some(OMMOD(dom)), Some(OMMOD(cod)))) => (dom, cod)
        case _ => return None
      }

    Some(LogrelType(mors, commonMorDomain, commonMorCodomain))
  }
}

object FlexaryLogrelOperator extends LogrelOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_operator")
}

object UnaryLogrelOperator extends LogrelOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unary_logrel_operator")
}