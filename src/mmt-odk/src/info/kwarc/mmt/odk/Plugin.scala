package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, MMTStructureChecker, Solver, SubtypingRule}
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory}
import info.kwarc.mmt.api.symbols._
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mitm.MitM
import info.kwarc.mmt.odk.OpenMath.CodingServer
import info.kwarc.mmt.odk.SCSCP.Server.MitMServer
import info.kwarc.mmt.odk.Singular.SingularImporter

class Plugin extends frontend.Extension {
  val theory = MitM.mathpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
    controller.extman.addExtension(new Sage.Plugin)
    controller.extman.addExtension(new SingularImporter)
    controller.extman.addExtension(new activecomp.Plugin)
    controller.extman.addExtension(new ODKGraph)
    controller.extman.addExtension(new MitMServer)
    controller.extman.addExtension(new UniverseInference)
    controller.extman.addExtension(new CodingServer)
    controller.extman.addExtension(new SubtypeGenerator)
    controller.extman.addExtension(MitM.preproc)
  }
}

class GAPSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("GAP",GAP.GAP._base,serverurl,port)
class SingularSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Singular",Singular.Singular.dpath,serverurl,port)
class SageSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Sage",Sage.Sage._base,serverurl,port)

object RemoteGAPSystem extends GAPSystem("neptune.eecs.jacobs-university.de")

class UniverseInference extends ChangeListener {

  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker])
  private object TypeLevel {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "TypedHierarchy") ? "Symbols" ? "TypeLevel"
    val term = OMS(path)
    def apply(i : BigInt) : Term = i match {
      case _ if i == BigInt(1) => OMS(Typed.ktype)
      case _ if i == BigInt(2) => OMS(Typed.kind)
      case _ if i < 1 =>
        require(i>=1)
        ???
      case _ =>
        OMA(this.term,List(NatLiterals.of(i)))
    }
    def unapply(t:Term) : Option[BigInt] = t match {
      case OMA(this.term,List(NatLiterals(i))) => i match {
        case i:BigInt => Some(i)
        case _ => None
      }
      case OMS(Typed.ktype) => Some(1)
      case OMS(Typed.kind) => Some(2)
      case _ => None
    }
  }

  private val default : BigInt = 100

  def getUniverse(e : StructuralElement) : BigInt = e match {
    case ds: Structure =>
      val dom: Option[DeclaredTheory] = ds.from match {
        case OMPMOD(mp, _) =>
          Some(controller.getAs(classOf[DeclaredTheory], mp))
        case _ => return default
      }
      // println("Structure " + ds.path + "with domain " + dom.map(_.path))
      dom.map(getUniverse).getOrElse(default)

    case th: DeclaredTheory =>
      th.metadata.get(TypeLevel.path).map(_.value).headOption match {
        case Some(TypeLevel(j)) => j
        case _ =>
          val decs = th.getDeclarations.map(getUniverse)
          if (decs.isEmpty) default else decs.max
      }
    case c : FinalConstant if c.tp.isDefined && c.df.isEmpty =>
      val parent = controller.get(c.parent)
      val parentcurrent = parent.metadata.get(TypeLevel.path).map(_.value)
      val context = parent match {
        case th : DeclaredTheory => th.getInnerContext
        case _ => return default
      }
      val univ = Solver.infer(controller, context, c.tp.get, None)
      val ret = univ match {
        case Some(TypeLevel(i)) =>
          i
        case _ => default
      }
      ret
    case _ : FinalConstant => 1
    case _ => 1
  }

  override def onCheck(c: StructuralElement) : Unit = c match {
    case c : FinalConstant =>
      c.tp match {
        case Some(tp) if c.df.isEmpty =>
          val parent = controller.get(c.parent)
          val parentcurrent = parent.metadata.get(TypeLevel.path).map(_.value)
          val previous : BigInt = parentcurrent.headOption match {
            case Some(TypeLevel(j)) => j
            case _ => 1
          }
          val newU = getUniverse(c)
          c.metadata.update(new MetaDatum(TypeLevel.path, TypeLevel(newU)))
          val ret = newU max previous
          parent.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(ret)))
        case _ =>
          val parent = controller.get(c.parent)
          if (parent.metadata.get(TypeLevel.path).isEmpty) parent.metadata.add(new MetaDatum(TypeLevel.path,TypeLevel(1)))
      }
    case ds : Structure =>
      val parent = controller.get(c.parent) match {
        case dm : DeclaredModule => dm
        case _ => return ()
      }
      val parentV = getUniverse(parent)
      val structV = getUniverse(ds)
      ds.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(parentV)))
      parent.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(parentV max structV)))
    case _ =>
  }
}

class SubtypeGenerator extends ChangeListener {
  override val logPrefix = "abbrev-rule-gen"
  protected val subtypeTag = "subtype_rule"

  private def rulePath(r: SubtypeJudgRule) = r.by / subtypeTag

  private def present(t: Term) = controller.presenter.asString(t)


  private def getGeneratedRule(p: Path): Option[SubtypeJudgRule] = {
    p match {
      case p: GlobalName =>
        controller.globalLookup.getO(p / subtypeTag) match {
          case Some(r: RuleConstant) => r.df.map(df => df.asInstanceOf[SubtypeJudgRule])
          case _ => None
        }
      case _ => None
    }
  }


  override def onAdd(e: StructuralElement) {
    onCheck(e)
  }

  override def onDelete(e: StructuralElement) {
    getGeneratedRule(e.path).foreach { r => controller.delete(rulePath(r)) }
  }

  override def onCheck(e: StructuralElement): Unit = e match {
    case c: Constant if c.tpC.analyzed.isDefined => c.tp match {
      case Some(subtypeJudg(tm1,tm2)) =>
        val rule = new SubtypeJudgRule(tm1,tm2,c.path)
        val ruleConst = RuleConstant(c.home,c.name / subtypeTag,subtypeJudg(tm1,tm2),Some(rule))
        ruleConst.setOrigin(GeneratedBy(this))
        log(c.name + " ~~> " + present(tm1) + " <: " + present(tm2))
        controller add ruleConst
      case _ =>
    }
    case _ =>
  }

}

class SubtypeJudgRule(val tm1 : Term, val tm2 : Term, val by : GlobalName) extends SubtypingRule {
  val head = subtypeJudg.path

  override def applicable(tp1: Term, tp2: Term): Boolean = tp1.hasheq(tm1) && tp2.hasheq(tm2)

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    solver.check(Equality(stack,tm1,tp1,None))
    solver.check(Equality(stack,tm2,tp2,None))
    Some(true)
  }
}

object subtypeJudg {
  val name = "subtypeJudge"
  val baseURI = LFX.ns / "Subtyping"
  val judgpath = baseURI ? "JudgmentSymbol"
  val path = judgpath ? name
  val term = OMS(path)
  def apply(t1 : Term, t2 : Term) = OMA(term,List(t1,t2))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(t1,t2)) => Some(t1,t2)
    case _ => None
  }
}