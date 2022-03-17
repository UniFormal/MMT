package info.kwarc.mmt.stex.features


/*

import info.kwarc.mmt.api.{GlobalName, InvalidElement, LocalName, ParametricRule, Path, Rule}
import info.kwarc.mmt.api.checking.{ExtendedCheckingEnvironment, SubtypingRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.notations.{LabelArg, LabelInfo, Marker, SimpArg}
import info.kwarc.mmt.api.objects.{Context, Free, FreeOrAny, OMA, OMBIND, OMID, OMMOD, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.symbols.{Constant, Declaration, DerivedDeclaration, Elaboration, RuleConstant, StructuralFeature, StructuralFeatureRule, Structure}
import info.kwarc.mmt.api.uom.{AbbrevRule, ExtendedSimplificationEnvironment}
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Pi}
import info.kwarc.mmt.odk.SubtypeJudgRule
import info.kwarc.mmt.stex.STeX

import scala.collection.mutable

class PillarFeature extends StructuralFeature("stex:pillar") {
  override def getHeaderNotation: List[Marker] = List(LabelArg(1, LabelInfo.none),SimpArg(1))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    dd.tp match {
      case Some(OMMOD(p)) =>
        val module = env.objectChecker.lookup.getTheory(p)
        module.getConstants.filter(_.rl.isEmpty).foreach {c =>
          dd.getConstants.find(_.name == c.name) match {
            case Some(ddc) if ddc.df.isDefined =>
            case None =>
              env.errorCont(InvalidElement(dd,"Missing assignment for " + c.name))
          }
        }
      case _ =>
        env.errorCont(InvalidElement(dd,"Header malformed"))
    }
  }

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment]): Elaboration = new Elaboration {
    def domain = Nil
    override def getO(name: LocalName): Option[Declaration] = None
  }

}

class DefinitionFeature extends StructuralFeature("stex:definition") {
  override def getHeaderNotation: List[Marker] = List(LabelArg(1, LabelInfo.none),SimpArg(1))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    dd.tp match {
      case Some(OMS(p)) =>
        val orig = env.objectChecker.lookup.getConstant(p)
        val vars = dd.getConstants.filter(_.rl.contains("variable"))
        val premises = dd.getConstants.filter(_.rl.contains("premise"))
        val defs = dd.getConstants.filter(_.rl.contains("definiens"))
        if (defs.length != 1) env.errorCont(InvalidElement(dd,"definition does not have exactly one definiens"))
      case _ =>
        env.errorCont(InvalidElement(dd,"Header malformed"))
    }
  }

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment]): Elaboration = new Elaboration {
    val orig = dd.tp match {
      case Some(OMS(p)) => controller.getO(p).asInstanceOf[Option[Constant]]
      case _ => None
    }
    val vars = dd.getConstants.filter(_.rl.contains("variable")).flatMap(c => c.metadata.getValues(STeX.meta_vardecl) match {
      case List(STeX.StringLiterals(s)) => Some((LocalName(s),c.tp)) // TODO types?
      case _ => None
    })
    val premises = dd.getConstants.filter(_.rl.contains("premise")).flatMap(_.df)
    val body = dd.getConstants.filter(_.rl.contains("definiens")) match {
      case List(c) => c.df match {
        case Some(OMBIND(OMS(ParseResult.unknown),_,bd)) => Some(bd)
        case d => d
      }
      case _ => None
    }
    val wprm = body.map(premises.foldRight(_)((t,bd) => STeX.universal_quantifier(LocalName("_"),Some(OMA(OMS(STeX.ded),List(t))),bd)))
    val df = wprm.map(vars.foldRight(_)((ln,bd) => STeX.universal_quantifier(ln._1,ln._2,bd)))

    val name = LocalName(dd.name + "rule")

    val domain = if (df.isDefined && orig.isDefined) orig.get.df match {
      case None if orig.get.parent == dd.parent =>
        controller.library.update(Constant(orig.get.home,orig.get.name,orig.get.alias,orig.get.tp,df,orig.get.rl,orig.get.notC))
        Nil
      case _ =>
        List(name)
    } else Nil

    override def getO(name: LocalName): Option[Declaration] = name match {
      case `name` =>
        Some(RuleConstant(dd.home,name,OMA(OMID(Path.parseM("scala://features.stex.mmt.kwarc.info?AbbreviationRule")),
          List(OMS(orig.get.path),df.get)),Some(new AbbrevRule(orig.get.path,df.get))))
      case _ => None
    }
  }

}

object Pillar extends StructuralFeatureRule(classOf[PillarFeature],"stex:pillar")

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
    lazy val domain = List(if (dd.name.toString.endsWith("-feature")) LocalName(dd.name.toString.dropRight(8)) else
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
 */