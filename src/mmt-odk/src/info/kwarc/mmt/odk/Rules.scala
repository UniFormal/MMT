package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, MMTStructureChecker, Solver, SubtypingRule}
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import SemanticOperator._
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.MitM.MitM

import scala.collection.mutable

object IntegerLiterals extends RepresentedRealizedType(MitM.z,MitM.Z)
object NatLiterals extends RepresentedRealizedType(MitM.n,MitM.N)
object PosLiterals extends RepresentedRealizedType(MitM.p,MitM.P)

object NatSucc extends RealizedOperator(MitM.succ, MitM.n =>: MitM.synType(MitM.n), Arithmetic.Succ, MitM.N =>: MitM.N)

/* FR: This is already part of Arithmetic.Succ
object NatSuccInverse extends InverseOperator(MitM.succ) {
  def unapply(l: OMLIT): Option[List[OMLIT]] = l match {
    case NatLiterals(u : BigInt) if u>0 => Some(List(NatLiterals.of(u-1)))
    case _ => None
  }
}
*/

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

  object ModelsOf extends LFRecSymbol("ModelsOf") {
    // val path2 = Records.path ? "ModelsOfUnary"
    // val term2 = OMS(path2)
    def apply(mp : MPath, args : Term*) = OMA(this.term,List(OMPMOD(mp,args.toList)))
    def apply(t : Term) = OMA(this.term,List(t))
    def unapply(t : Term) : Option[Term] = t match {
      case OMA(this.term, List(tm)) => Some(tm)
      // case OMA(this.term2,List(OMMOD(mp))) => Some(OMMOD(mp))
      case OMA(this.term, OMMOD(mp) :: args) => Some(OMPMOD(mp,args))
      case _ => None
    }
  }

  object Getfield extends LFRecSymbol("Getfield") {
    def apply(t:Term, f: LocalName) = OMA(this.term, List(t, OML(f)))
    def unapply(t: Term) : Option[(Term,LocalName)] = t match {
      case OMA(this.term, List(tm, OML(f,_,_,_,_))) => Some((tm, f))
      case _ => None
    }
  }

  object Lists {
    val baseURI = LFX.ns / "Datatypes"
    val th = baseURI ? "ListSymbols"
  }

  object ListType {
    val path = Lists.th ? "ListType"
    val term = OMS(path)
    def apply(tp : Term) : Term = OMA(this.term,List(tp))
    def unapply(tp : Term) : Option[Term] = tp match {
      case OMA(this.term,List(t)) => Some(t)
      case _ => None
    }
  }

  object ListNil {
    val path = Lists.th ? "nil"
    val term = OMS(path)
  }

  object Append {
    val path2 = Lists.th ? "list"
    val term2 = OMS(path2)
    val path = Lists.th ? "append"
    val term = OMS(path)
    def apply(a: Term, ls : Term) : Term = OMA(this.term,List(a,ls))
    def unapply(tm : Term) : Option[(Term,Term)] = tm match {
      case OMA(this.term,List(a,ls)) => Some((a,ls))
      case OMA(this.term2,args) if args.nonEmpty =>
        if (args.length==1) Some((args.head,ListNil.term))
        else Some((args.head,OMA(this.term2,args.tail)))
      case _ => None
    }
  }

  object LFList {
    val path = Lists.th ? "list"
    val term = OMS(path)
    def apply(tms : List[Term]) : Term = OMA(this.term,tms)
    def unapply(ls : Term) : Option[List[Term]] = ls match {
      case OMA(this.term,args) => Some(args)
      case Append(a,lsi) => unapply(lsi).map(a :: _)
      case ListNil.term => Some(Nil)
      case _ => None
    }
  }

  object Map {
    val path = Lists.th ? "map"
    val term = OMS(path)
    def apply(ls: Term, f : Term) : Term = OMA(this.term,List(ls,f))
    def unapply(tm : Term) : Option[(Term,Term)] = tm match {
      case OMA(this.term,List(ls,f)) => Some((ls,f))
      case _ => None
    }
  }

  object SigmaTypes {
    val baseURI = LFX.ns / "Sigma"
    val thname = "Symbols"
    val path = baseURI ? thname
    def lfssymbol(name : String) = path ? name
  }

  class LFSigmaSymbol(name:String) {
    val path = SigmaTypes.path ? name
    val term = OMS(path)
  }

  object Sigma extends LFSigmaSymbol("Sigma") {
    def apply(name : LocalName, tp : Term, body : Term) = OMBIND(this.term, OMV(name) % tp, body)
    def apply(con: Context, body : Term) = OMBIND(this.term, con, body)
    def unapply(t : Term) : Option[(LocalName,Term,Term)] = t match {
      case OMBIND(OMS(this.path), Context(VarDecl(n,None,Some(a),None,_), rest @ _*), s) =>
        val newScope = if (rest.isEmpty)
          s
        else
          apply(Context(rest:_*), s)
        Some(n,a,newScope)
      case OMA(Product.term,args) if args.length >= 2 =>
        val name = OMV.anonymous
        if (args.length > 2)
          Some((name, args.head, OMA(Product.term, args.tail)))
        else
          Some((name,args.head,args.tail.head))
      case _ => None
    }
  }

  object Product extends LFSigmaSymbol("Product") {
    def apply(t1 : Term, t2 : Term) = OMA(this.term,List(t1,t2))
    def apply(in: List[Term], out: Term) = if (in.isEmpty) out else OMA(this.term, in ::: List(out))
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl.init, tl.last)))
      case _ => None
    }
  }

  object Tuple extends LFSigmaSymbol("Tuple") {
    def apply(t1 : Term, t2 : Term) = OMA(this.term,List(t1,t2))
    def apply(in: List[Term]) = OMA(this.term, in)
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(this.term,ls) if ls.length==2 => Some(ls.head,ls(1))
      case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl)))
      case _ => None
    }
  }
}

class UniverseInference extends ChangeListener {

  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker])
  private object TypeLevel {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "TypedHierarchy") ? "Symbols" ? "TypeLevel"
    val term = OMS(path)
    def apply(i : BigInt) : Term = i match {
      case _ if i == BigInt(1) || i == BigInt(0) => OMS(Typed.ktype)
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

  private def default : BigInt = {
    print("")
    1000
  }

  def getUniverse(e : StructuralElement) : BigInt = e match {
    case ds: Structure =>
      val dom: Option[Theory] = ds.from match {
        case OMPMOD(mp, _) =>
          Some(controller.getAs(classOf[Theory], mp))
        case _ => return default
      }
      // println("Structure " + ds.path + "with domain " + dom.map(_.path))
      dom.map(getUniverse).getOrElse(default)

    case th:Theory =>
      th.metadata.get(TypeLevel.path).map(_.value).headOption match {
        case Some(TypeLevel(j)) => j
        case _ =>
          val decs = th.getDeclarations.map(getUniverse)
          val ret = if (decs.isEmpty) BigInt(0) else decs.max
          th.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(ret)))
          ret
      }
    case c : FinalConstant if c.tp.isDefined && c.df.isEmpty =>
      val parent = controller.get(c.parent)
      // val parentcurrent = parent.metadata.get(TypeLevel.path).map(_.value)
      val context = parent match {
        case th : Theory => th.getInnerContext
        case _ => return 0
      }
      val univ = try { Solver.infer(controller, context, c.tp.get, None) } catch {
        case e : LookupError =>
          println(controller.presenter.objectLevel.asString(c.tp.get))
          println(c.tp.get)
          throw e
      }
      val ret = univ match {
        case Some(TypeLevel(i)) =>
          i
        case _ => default
      }
      ret
    case _ : FinalConstant => 0
    case _ => 0
  }

  override def onCheck(e: StructuralElement) : Unit = e match {
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
      val parent = controller.get(ds.parent) match {
        case m : Module => m
        case _ => return
      }
      val parentcurrent = parent.metadata.get(TypeLevel.path).map(_.value)
      val previous : BigInt = parentcurrent.headOption match {
        case Some(TypeLevel(j)) => j
        case _ => 1
      }
      val structV = getUniverse(ds)
      ds.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(structV)))
      parent.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(previous max structV)))
    case _ =>
  }
}

class SubtypeGenerator extends ChangeListener {
  override val logPrefix = "subtype-rule-gen"
  protected val subtypeTag = "subtype_rule"

  private def rulePath(r: SubtypeJudgRule) = r.by.map(_ / subtypeTag)

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

  override def onAdd(e: StructuralElement): Unit = {
    onCheck(e)
  }

  override def onDelete(e: StructuralElement): Unit = {
    getGeneratedRule(e.path).foreach { r => rulePath(r).foreach(controller.delete) }
  }

  override def onCheck(e: StructuralElement): Unit = e match {
    case c: Constant if c.tpC.analyzed.isDefined => c.tp match {
      case Some(subtypeJudg(tm1,tm2)) =>
        val rule = new SubtypeJudgRule(tm1,tm2,Some(c.path))
        val ruleConst = RuleConstant(c.home,c.name / subtypeTag,subtypeJudg(tm1,tm2),Some(rule))
        ruleConst.setOrigin(GeneratedFrom(c.path, this))
        log(c.name.toString + " ~~> " + present(tm1) + " <: " + present(tm2))
        controller add ruleConst
      case _ =>
    }
    case _ =>
  }
}

class SubtypeJudgRule(val tm1 : Term, val tm2 : Term, val by : Option[GlobalName]) extends SubtypingRule {
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
