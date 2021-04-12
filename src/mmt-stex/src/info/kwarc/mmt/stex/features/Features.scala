package info.kwarc.mmt.stex.features


import info.kwarc.mmt.api.{GlobalName, LocalName, ParametricRule, Rule}
import info.kwarc.mmt.api.checking.{ExtendedCheckingEnvironment, SubtypingRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.notations.{LabelArg, LabelInfo}
import info.kwarc.mmt.api.objects.{Context, Free, FreeOrAny, OMA, OMBIND, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, Elaboration, StructuralFeature, StructuralFeatureRule, Structure}
import info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Pi}
import info.kwarc.mmt.odk.SubtypeJudgRule
import info.kwarc.mmt.stex.STeX

import scala.collection.mutable

class TheoremFeature extends StructuralFeature("stex:theorem") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    var conclusions = 0
    dd.getDeclarations.forall{
      case d : Constant if d.rl.contains("Variable") => true
      case d : Constant if d.rl.contains("conclusion") => conclusions += 1 ; true
      case _ =>
        ???
    } && conclusions == 1
  }

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None): Elaboration = new Elaboration {
    lazy val domain = List(if (dd.name.toString.endsWith("_feature")) LocalName(dd.name.toString.dropRight(8)) else
      ???
    )

    lazy val variables = dd.getConstants.collect {
      case c: Constant if c.rl contains "Variable" =>
        c.metadata.get(STeX.meta_quantification).map(_.value) match {
          case OMS(p) :: Nil =>
            (c.name,c.tp match {
              case Some(Free(_,t)) => Some(t)
              case Some(t) => Some(t)
              case _ => None
            },p)
          case _ =>
            ???
        }
    }

    lazy val constant = dd.getConstants.collectFirst {
      case c if c.rl.contains("conclusion") =>
        val tm = c.df match {
          case Some(Free(_,t)) => t
          case Some(t) => t
          case None =>
            ???
        }
        val tp = variables.foldLeft(tm)((t,tr) => OMBIND(OMS(tr._3),VarDecl(tr._1,None,tr._2,None,None),t))
        val ntp = OMA(OMS(STeX.ded),tp :: Nil)
        Constant(parent.toTerm, domain.head, c.alias, Some(FreeOrAny(ntp.freeVars,ntp)), None, None)
    }.getOrElse{
      ???
    }

    def getO(name: LocalName) = if (name == constant.name) Some(constant) else None
  }
}

object Theorem extends StructuralFeatureRule(classOf[TheoremFeature],"stex:theorem")

object SubtypeRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm1,tm2) =>
      new SubtypeJudgRule(tm1,tm2,None)
    case _ =>
      ???
  }
}

/*
class SubtypeJudgRule(val tm1 : Term, val tm2 : Term, val by : GlobalName) extends SubtypingRule {
  val head = subtypeJudg.path
  def applicable(tp1: Term, tp2: Term): Boolean = tp1.hasheq(tm1) && tp2.hasheq(tm2)
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    Some(true)
  }
}

 */