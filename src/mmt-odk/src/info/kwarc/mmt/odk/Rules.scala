package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, MMTStructureChecker, Solver, SubtypingRule}
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk.Singular.SingularImporter
import info.kwarc.mmt.MiTM.MitM._
import SemanticOperator._
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory}
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant, RuleConstant, Structure}
import info.kwarc.mmt.MiTM.MitM

import scala.collection.mutable

object IntegerLiterals extends RepresentedRealizedType(z,Z)
object NatLiterals extends RepresentedRealizedType(n,N)
object PosLiterals extends RepresentedRealizedType(p,P)

object NatSucc extends RealizedOperator(succ, n =>: n, Arithmetic.Succ, N =>: N)

object NatSuccInverse extends InverseOperator(MitM.succ) {
  def unapply(l: OMLIT): Option[List[OMLIT]] = l match {
    case NatLiterals(u : BigInt) if u>0 => Some(List(NatLiterals.of(u-1)))
    case _ => None
  }
}

object StringLiterals extends RepresentedRealizedType(OMS(MitM.string),StandardString)

object IntegerSubtype extends SubtypingRule {
  val head = MitM.int
  def applicable(tp1: Term, tp2: Term): Boolean = (tp1,tp2) match {
    case (NatLiterals.synType,IntegerLiterals.synType) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = (tp1,tp2) match {
    case (NatLiterals.synType,IntegerLiterals.synType) => Some(true)
    case _ => None
  }
}

object LFX {
  val ns = DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX"
  class LFRecSymbol(name:String) {
    val path = (ns / "Records") ? "Symbols" ? name
    val term = OMS(path)
  }

  case class RecordBody(self: Option[LocalName], fields: List[OML]) {
    /** names of all fields */
    def names = fields.map(_.name)
    /** checks for duplicate names */
    def hasDuplicates = utils.hasDuplicates(names)
    /** retrieve a field for a given name */
    def get(l: LocalName) = fields.find(_.name == l)
  }

  /** unifies record terms and types; the empty record is OMA(this.term,Nil), not this.term */
  class RecordLike(n: String) extends LFRecSymbol(n) {
    // there may not be an apply method that takes a context instead of a List[OML]
    def apply(v:OML*): Term = apply(None, v.toList)
    def apply(self: Option[LocalName], fields: List[OML]): Term = {
      self match {
        case None => OMA(this.term, fields)
        case Some(l) => OMBINDC(this.term, Context(VarDecl(l)), fields)
      }
    }
    def unapply(t : Term) : Option[RecordBody] = t match {
      case OMA(this.term, OMLList(fs)) => Some(RecordBody(None, fs))
      case OMBINDC(this.term, Context(VarDecl(n, None, None, None, _), rest@_*), OMLList(fs)) if rest.isEmpty => Some(RecordBody(Some(n), fs))
      case _ => None
    }
  }

  object RecExp extends RecordLike("Recexp")
}

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
  override val logPrefix = "subtype-rule-gen"
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
  def applicable(tp1: Term, tp2: Term): Boolean = tp1.hasheq(tm1) && tp2.hasheq(tm2)
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
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