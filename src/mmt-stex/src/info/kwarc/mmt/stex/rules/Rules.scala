package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.checking.{CheckingCallback, CheckingUnit, ComputationRule, History, InferenceRule, SingleTermBasedCheckingRule, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.{ElaborationOf, GlobalName, LocalName, ParametricRule, ParseError, Rule, RuleSet}
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.symbols.{Constant, Structure}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, Simplifiability, Simplify, StandardInt, StandardNat, StandardString}
import info.kwarc.mmt.stex.Extensions.{SHTMLContentManagement, Symbols}
import info.kwarc.mmt.stex.xhtml.{SHTMLNode, SHTMLObject, SHTMLState, SemanticState}
import info.kwarc.mmt.stex.{IsSeq, SCtx, SHTML, SHTMLHoas, SOMBArg, STerm}

object StringLiterals extends RepresentedRealizedType(OMS(SHTML.string),StandardString)
object IntLiterals extends RepresentedRealizedType(OMS(SHTML.int),StandardInt)

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.objects.Conversions._

object Rules {

  def getBoundVars(tm : Term) : (List[(LocalName,Option[Term])],Term) = tm match {
    case SHTMLHoas.bound(_,_,x,bd) =>
      val ret = getBoundVars(bd)
      ((x.name,x.tp) :: ret._1,ret._2)
    case _ => (Nil,tm)
  }
}
class Pattern(original : Term) {
  private val (variablesI,bodyI) = Rules.getBoundVars(original)
  val body = bodyI
  val variables = variablesI.filterNot(v => original.freeVars.contains(v._1))
  val arity = variables.length
  def apply(args : List[Term]) = if (args.length == arity) {
    body ^? Substitution(variables.indices.map(i => variables(i)._1 / args(i)):_*)
  } else {
    ???
  }
  val head = original.head.get.asInstanceOf[GlobalName]

  // TODO
  private def recurse(tmmatch : Term,tmorig : Term)(cont : Context) : Option[List[(LocalName,Term)]] = (tmmatch,tmorig) match {
    case (OMID(s1),OMID(s2)) if s1 == s2 => Some(Nil)
    case (SHTML.flatseq(ls),OMV(x)) if variables.exists(_._1 ==x) =>
      Some(ls.map(t => (x,t)))
    case (_,OMV(x)) if variables.exists(_._1 == x) => Some(List((x,tmmatch)))
    case (OMA(t1,a1),OMA(t2,a2)) if a1.length == a2.length =>
      recurse(t1,t2)(cont) match {
        case Some(init) =>
          a1.indices.foldLeft[Option[List[(LocalName,Term)]]](Some(init))((ret,i) => ret match {
            case Some(r) =>
              recurse(a1(i),a2(i))(cont).map(r ::: _)
            case None => None
          })
        case _ => None
      }
    case _ =>
      print("")
      None
  }
  def unapply(tm : Term) = {
    recurse(tm,body)(Context.empty)
  }

  def instantiate(ls : List[(LocalName,Term)],tm : Term = original) : Option[Term] = tm match {
    case SHTMLHoas.bound(_,_,x,bd) if ls.exists(_._1 == x.name) => instantiate(ls,bd ^? (x.name / ls.find(_._1 == x.name).get._2))
    case SHTMLHoas.bound(_,_,_,_) => None
    case _ => Some(ls.foldLeft(tm)((t,p) => t ^? (p._1 / p._2)))
  }
}

trait UsesPatterns extends SingleTermBasedCheckingRule {
  protected val pattern : Pattern
  override def applicable(t: Term): Boolean = t match {
    case pattern(_) => true
    case _ => false
  }
}


object HOASRule extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = args match {
    case List(OMS(app),OMS(lambda),OMS(pi)) =>
      SHTMLHoas.HoasRule(app,lambda,pi)
    case _ =>
      throw ParseError("needs three OMS arguments")
  }
}

abstract class RulerRule extends Rule {
  def apply(controller: Controller, cont: Term => Option[HasMetaData], term: Term) : Option[HasMetaData]
}

object FieldRuler extends RulerRule {

  def doTp(tm : Term,field:LocalName)(implicit controller : Controller, cont: Term => Option[HasMetaData]) : Option[HasMetaData] = {
    val cu = CheckingUnit(None, Context.empty, Context.empty, null)
    val solver = new Solver(controller, cu, MathStructureRule)
    val rec = RecordsGeneral.makeRecBody(solver, tm)(Stack.empty, new History(Nil))
    rec._2 match {
      case Some(sm: SimpleModule) =>
        sm.getOrig(field)(controller.globalLookup, new History(Nil)) match {
          case Some(d) =>
            cont(d.toTerm) match {
              case s@Some(_) => s
              case _ => Some(d)
            }
          case _ => None
        }
      case Some(mt : MergedType) =>
        mt.getOrig(field)(controller.globalLookup, new History(Nil)) match {
          case Some(d) =>
            cont(d.toTerm) match {
              case s@Some(_) => s
              case _ => Some(d)
            }
          case _ => None
        }
      case Some(_) =>
        None
      case None =>
        None
    }
  }
  override def apply(controller: Controller, cont: Term => Option[HasMetaData], term: Term): Option[HasMetaData] = term match {
    case Getfield(SHTML.of_type(_,RecMerge(ls)),fieldname) =>
      val nls = ls.filter {
        case OMS(_) => true
        case ModelsOf(_) => true
        case _ => false
      }
      val ret = nls match {
        case List(x) => x
        case ls => RecMerge(ls:_*)
      }
      doTp(ret,fieldname)(controller,cont)
    case Getfield(SHTML.of_type(_,rtp), fieldname) =>
      doTp(rtp,fieldname)(controller,cont)
    case _ => None
  }
}

object StructureRuler extends RulerRule {
  override def apply(controller: Controller, cont: Term => Option[HasMetaData], term: Term): Option[HasMetaData] = term match {
    case OMS(a) =>
      controller.getO(a) match {
        case Some(c: Constant) if c.name.length > 1 =>
          c.getOrigin match {
            case ElaborationOf(source) =>
              controller.getO(source) match {
                case Some(s: Structure) =>
                  s.getMostSpecific(c.name.drop(1)) match {
                    case Some((c: Constant, _)) =>
                      SHTMLContentManagement.getHead(c) match {
                        case Some(OMS(np)) =>
                          controller.getO(np)
                        case _ => Some(c)
                      }
                    case None =>
                      s.getDeclarations.find(_.name.endsWith(c.name.drop(1))) match {
                        case Some(c: Constant) =>
                          SHTMLContentManagement.getHead(c) match {
                            case Some(OMS(np)) =>
                              controller.getO(np)
                            case _ => Some(c)
                          }
                        case _ => Some(c)
                      }
                    case _ => Some(c)
                  }
                case _ => Some(c)
              }
            case _ => Some(c)
          }
        case _ => None
      }
    case _ => None
  }
}

// Terms -> Variables

trait BindingRule extends Rule {
  def apply[SHTMLClass<: SHTMLObject](tm : Term,assoc:Boolean)(implicit state : SHTMLState[SHTMLClass], self:SHTMLClass) : Option[Context]
}

object InformalBindingRule extends BindingRule {
  def apply[SHTMLClass<: SHTMLObject](tm : Term,assoc:Boolean)(implicit state : SHTMLState[SHTMLClass], self:SHTMLClass) : Option[Context] = tm match {
    case SHTML.flatseq(ls) if assoc && ls.forall(_.isInstanceOf[OMV]) =>
      Some(ls.flatMap(apply(_,false)).flatten)
    case OMV(x) =>
      val vd = self.getVariableContext.findLast(_.name == x).getOrElse {
        val v = state.markAsUnknown(OMV(state.getUnknownTp))
        VarDecl(x,tp=v)
      }
      vd.metadata.getValues(SHTML.flatseq.sym) match {
        case OMS(SHTML.flatseq.sym) :: _ =>
          Some(Context(vd.copy(tp = vd.tp.map(tm => SHTML.flatseq.tp(tm)))))
        case _ => Some(Context(vd))
      }
    case _ => None
  }
}

case class BinderRule(pattern : Pattern,head : GlobalName,tp : Option[LocalName]) extends BindingRule with UsesPatterns {
  def apply[SHTMLClass<: SHTMLObject](tm : Term,assoc:Boolean)(implicit state : SHTMLState[SHTMLClass], self:SHTMLClass) : Option[Context] = tm match {
    case pattern(ls) =>
      val (vars, tpO) = tp match {
        case Some(tpn) =>
          val p = ls.find(_._1 == tpn).getOrElse {
            return None
          }
          (ls.filterNot(_ == p), Some(p._2))
        case _ => (ls,None)
      }
      Some(vars.foldLeft(Context.empty) {
        case (ctx, (_, OMV(ln))) =>
          tpO match {
            case Some(t) => ctx ++ (ln % t)
            case _ => ctx ++ VarDecl(ln)
          }
        case (ctx, (_, ot)) =>
          val nc = state.makeBinder(ot,assoc).variables.map {
            case vd if vd.tp.isEmpty =>
              tpO match {
                case Some(t) => vd.name % t
                case _ => vd
              }
            case vd => vd
          }
          ctx ++ nc
      }.map{vd =>
        self.getVariableContext.findLast(_.name == vd.name).foreach(v => vd.copyFrom(v))
        tm.metadata.get(Symbols.meta_notation).foreach(x => vd.metadata.add(x))
        vd
      })
    case _ =>
      None
  }
}

case class TypingPropRule(pattern : Pattern,hhead : GlobalName,tp : Option[LocalName]) extends InferenceRule(hhead,SHTML.judgmentholds.sym) with UsesPatterns {
  override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case pattern(ls) =>
      tp match {
        case Some(tpn) =>
          val p = ls.find(_._1 == tpn).getOrElse {
            return None
          }
          solver.check(info.kwarc.mmt.api.objects.Inhabitable(stack,p._2))
          if (!covered) solver.inferType(ls.head._2)
          Some(OMS(SHTML.prop))
        case _ => None
      }
    case _ => None
  }
}

object TypingLike extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    if (args.length != 1 && args.length != 2) throw ParseError("one or two arguments expected")
    val (pattern,ln) = args match {
      case List(h,OMV(n)) =>
        (new Pattern(h),Some(n))
      case List(h,SHTML.binder(vd,OMV(m))) if vd.name == m =>
        (new Pattern(h),Some(m))
      case List(h) =>
        (new Pattern(h),None)
      case _ =>
        throw ParseError("second argument needs to be a variable")
    }
    val head = pattern.body.head match {
      case Some(gn : GlobalName) => gn
      case _ => SHTML.informal.sym
    }
    RuleSet(BinderRule(pattern,head,ln),TypingPropRule(pattern,head,ln))
  }
}

object TypeAssertionLike extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    if (args.length != 1 && args.length != 2) throw ParseError("one or two arguments expected")
    val (pattern,ln) = args match {
      case List(h,OMV(n)) =>
        (new Pattern(h),Some(n))
      case List(h,SHTML.binder(vd,OMV(m))) if vd.name == m =>
        (new Pattern(h),Some(m))
      case List(h) =>
        (new Pattern(h),None)
      case _ =>
        throw ParseError("second argument needs to be a variable")
    }
    val head = pattern.body.head match {
      case Some(gn : GlobalName) => gn
      case _ => SHTML.informal.sym
    }
    RuleSet(BinderRule(pattern,head,ln),TypeAssertionRule(pattern,head,ln))
  }

  case class TypeAssertionRule(pattern: Pattern, hhead: GlobalName, tp: Option[LocalName]) extends ComputationRule(hhead) with UsesPatterns {

    override def applicable(t: Term): Boolean = pattern.unapply(t).nonEmpty
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
      case pattern(List((_,t),(_,tp))) =>
        check.check(Typing(stack,t,tp))
        Simplify(t)
      case _ => Simplifiability.NoRecurse
    }
  }
}

// HTML -> OMDoc

trait HTMLTermRule extends Rule {
  def apply(tm : Term)(implicit state : SHTMLState[SHTMLNode], self:SHTMLNode,cont:Term => Term) : Option[Term]
}

object MnRule extends HTMLTermRule {
  override def apply(tm : Term)(implicit state : SHTMLState[SHTMLNode], self:SHTMLNode,cont:Term => Term) : Option[Term] = tm match {
    case SHTML.informal(n) if n.label == "mi" && n.child.length == 1 && n.attribute("mathvariant").exists(_.head.toString() == "normal") =>
      val str = n.child.head.toString()
      if (str.forall(_.isDigit)) {
        Some(IntLiterals.parse(str))
      } else None
    case _ => None
  }
}

object ParenthesisRule extends HTMLTermRule {
  val open = new {
    def unapply(tm : Term) = tm match {
      case SHTML.informal(o) if o.label == "mo" && o.attribute("class").exists(_.exists(_.toString() == "opening")) &&
        o.child.length == 1 && o.child.head.toString() == "(" => Some(true)
      case _ => None
    }
  }
  val close = new {
    def unapply(tm : Term) = tm match {
      case SHTML.informal(o) if o.label == "mo" && o.attribute("class").exists(_.exists(_.toString() == "closing")) &&
        o.child.length == 1 && o.child.head.toString() == ")" => Some(true)
      case _ => None
    }
  }
  override def apply(tm : Term)(implicit state : SHTMLState[SHTMLNode], self:SHTMLNode,cont:Term => Term) : Option[Term] = tm match {
    case SHTML.informal.op("mrow",List(open(_),t,close(_))) =>
      Some(SHTML.parens(t))
    case _ => None
  }
}

object ParensIdentityRule extends ComputationRule(SHTML.parens.sym) {
  override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case SHTML.parens(t) => Simplify(t)
    case _ => Simplifiability.NoRecurse
  }
}

object MiMoVariableRule extends HTMLTermRule {
  override def apply(tm: Term)(implicit state : SHTMLState[SHTMLNode], self:SHTMLNode,cont:Term => Term): Option[Term] = tm match {
    case SHTML.informal(n) if n.label == "mi" && n.child.length == 1 && n.child.head.toString().length <= 2 && MnRule(tm).isEmpty =>
      Some(OMV(n.child.head.toString()))
    case _ => None
  }
}

object OMVRule extends HTMLTermRule {
  override def apply(tm: Term)(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode,cont:Term => Term): Option[Term] = tm match {
    case OMV(x) =>
    self.getVariableContext.findLast(_.name == x) match {
      case Some(vd) =>
        vd.metadata.get(SHTML.flatseq.sym) match {
          case Nil => None
          case ls =>
            tm.metadata.add(ls:_*)
            Some(tm)
          case _ => None
        }
      case _ => None
    }
    case _ => None
  }
}

object Reorder extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = {
    args match {
      case List(OMS(p),StringLiterals(s)) if s.forall(c => c.isDigit || c == ',') =>
        ReorderRule(p,s)
      case _ =>
        ???
    }
  }
  case class ReorderRule(sym : GlobalName,str : String) extends Rule {
  }
}

object ReorderRule extends HTMLTermRule {

  def applyOMA(orig: Term, ints:List[Int], h: Option[SHTMLHoas.HoasRule], f: Term, args: List[Term])(implicit state: SHTMLState[SHTMLNode]): Option[Term] = {
    //val ints = str.split(',').map(_.toInt).toList
    if (args.length == ints.length) {
      val nargs = ints.map(i => args(i - 1))
      val ret = SHTMLHoas.OmaSpine(h, f, nargs)
      orig.metadata.getAll.foreach(ret.metadata.add(_))
      SHTMLContentManagement.addReorder(ints.map(_.toString).mkString(","), ret)
      Some(ret)
    } else None
  }

  def applyOMB(orig: Term, ints:List[Int], h: SHTMLHoas.HoasRule, f: Term, args: List[SOMBArg])(implicit state: SHTMLState[SHTMLNode]): Option[Term] = {
    //val ints = str.split(',').map(_.toInt).toList
    if (args.length == ints.length) {
      val nargs = ints.map(i => args(i - 1))
      val ret = h.HOMB(f, nargs)
      orig.metadata.getAll.foreach(ret.metadata.add(_))
      SHTMLContentManagement.addReorder(ints.map(_.toString).mkString(","), ret)
      Some(ret)
    } else None
  }
  override def apply(tm: Term)(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode,cont:Term => Term): Option[Term] = tm match {
    case SHTMLHoas.OmaSpine(h, f, args) =>
      state.getRuler(f) match {
        /*
        case Some(obj : Constant) =>
          val ruleOpt = RuleSet.collectRules(state.server.ctrl, self.getRuleContext).getOrdered(classOf[Reorder.ReorderRule]).collectFirst {
            case r@Reorder.ReorderRule(p, str) if p == obj.path => str
          }
          ruleOpt.flatMap(applyOMA(tm, _,h, f, args)) */
        case Some(o) =>
          SHTMLContentManagement.getReorder(o) match {
            case Some(ls) =>
              applyOMA(tm,ls,h,f,args)
            case _ => None
          }
        case _ => None
      }
    case SHTMLHoas.Omb(h,f,args) =>
      state.getRuler(f) match {
        /*
        case Some(obj: Constant) =>
          val ruleOpt = RuleSet.collectRules(state.server.ctrl, self.getRuleContext).getOrdered(classOf[Reorder.ReorderRule]).collectFirst {
            case r@Reorder.ReorderRule(p, str) if p == obj.path => str
          }
          ruleOpt.flatMap(applyOMB(tm, _,h, f, args))
       */
        case Some(o) =>
          SHTMLContentManagement.getReorder(o) match {
            case Some(ls) =>
              applyOMB(tm, ls, h, f, args)
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }
}

object ImplicitsRule extends HTMLTermRule {
  override def apply(tm: Term)(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode,cont:Term => Term): Option[Term] =
    if (tm.metadata.getValues(SHTML.implicit_binder.path).isEmpty) tm match {
    case SHTMLHoas.OmaSpine(h, f, args) =>
      state.getRuler(f) match {
        case Some(c : Constant) if c.tp.isDefined => c.tp match {
          case Some(SHTML.implicit_binder.spine(ctx,_)) =>
            val ret = SHTMLHoas.OmaSpine(h,f,ctx.map(_ => state.markAsUnknown(OMV(state.getUnknown))) ::: args) //doBinr(h, f, args)
            tm.metadata.getAll.foreach(ret.metadata.add(_))
            ret.metadata.update(SHTML.implicit_binder.path,IntLiterals(ctx.length))
            Some(ret)
          case _ => None
        }
        case _ => None
      }
    case SHTMLHoas.Omb(h, f, args) =>
      state.getRuler(f) match {
        case Some(c: Constant) if c.tp.isDefined => c.tp match {
          case Some(SHTML.implicit_binder.spine(ctx, _)) =>
            val ret = h.HOMB(f, ctx.map(_ => STerm(state.markAsUnknown(OMV(state.getUnknown)))) ::: args) //doBinr(h, f, args)
            tm.metadata.getAll.foreach(ret.metadata.add(_))
            ret.metadata.update(SHTML.implicit_binder.path, IntLiterals(ctx.length))
            Some(ret)
          case _ => None
        }
        case _ => None
      }
    /*case t@OMS(_) => TODO!!
      state.getRuler(t) match {
        case Some(c: Constant) if c.tp.isDefined => c.tp match {
          case Some(SHTML.implicit_binder.spine(ctx, _)) =>
            val h = SHTMLHoas.get(c)
            val ret = SHTMLHoas.OmaSpine(h,t,ctx.map(_ => state.markAsUnknown(OMV(state.getUnknown))))
            t.metadata.getAll.foreach(ret.metadata.add(_))
            Some(ret)
          case _ => None
        }
        case _ => None
      }*/
    case _ => None
  } else None
}

trait AssocRule extends Rule {
  val path : GlobalName
}
object PreRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = {
    args match {
      case List(OMS(p)) => RuleSet(PreRule(p),PreEqualRule.PreEqualityRule(p))
      case _ =>
        ???
    }
  }
  case class PreRule(path : GlobalName) extends AssocRule
}
object ConjRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = {
    args match {
      case List(OMS(p)) => ConjRule(p)
      case _ =>
        ???
    }
  }
  case class ConjRule(path : GlobalName) extends AssocRule
}
object BinRRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = {
    args match {
      case List(OMS(p)) => BinRRule(p)
      case _ =>
        ???
    }
  }
  case class BinRRule(path : GlobalName) extends AssocRule
}

object AssocRule extends HTMLTermRule {
  override def priority: Int = 10
  override def apply(tm: Term)(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode, cont: Term => Term): Option[Term] = tm match {
    case SHTMLHoas.OmaSpine(h, f, args) =>
      state.getRuler(f) match {
        /*
        case Some(c:Constant) =>
          RuleSet.collectRules(state.server.ctrl,self.getRuleContext).getOrdered(classOf[AssocRule]).collectFirst{
            case a if a.path == c.path => a
          } match {
            case Some(PreRule.PreRule(_)) =>
              ???
            case Some(BinRRule.BinRRule(_)) =>
              val ret = doBinr(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some(ConjRule.ConjRule(_)) =>
              val ret = doConj(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case _ => None
          } */
        case Some(obj) =>
          SHTMLContentManagement.getAssoctype(obj) match {
            case Some("binr" | "bin") =>
              val ret = doBinr(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some("conj") =>
              val ret = doConj(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some("pre") =>
              ???
            case _ => None
          }
        case _ => None
      }
    case SHTMLHoas.Omb(h, f, args) =>
      state.getRuler(f) match {
        /*
        case Some(c: Constant) =>
          RuleSet.collectRules(state.server.ctrl, self.getRuleContext).getOrdered(classOf[AssocRule]).collectFirst {
            case a if a.path == c.path => a
          } match {
            case Some(PreRule.PreRule(_)) =>
              val ret = doPre(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some(BinRRule.BinRRule(_)) =>
              ???
            case Some(ConjRule.ConjRule(_)) =>
              ???
            case _ => None
          } */
        case Some(obj) =>
          SHTMLContentManagement.getAssoctype(obj) match {
            case Some("binr" | "bin") =>
              ???
            case Some("pre") =>
              val ret = doPre(h, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some("conj") =>
              ???
            case _ => None
          }
        case _ => None
      }
    case OMA(f, args) =>
      state.getRuler(f) match {
        case Some(obj) =>
          SHTMLContentManagement.getAssoctype(obj) match {
            case Some("binr" | "bin") =>
              val ret = doBinr(None, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some("conj") =>
              val ret = doConj(None, f, args)
              ret.copyFrom(tm)
              Some(ret)
            case Some("pre") =>
              ???
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }
  def doPre(h :  SHTMLHoas.HoasRule,f : Term,args:List[SOMBArg])(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode) = args match {
    case STerm(v@OMV(_)) :: SCtx(ctx) :: rest if ctx.nonEmpty && state.isUnknown(v) =>
      ctx.variables.init.foldRight(
        h.HOMB(f, STerm(v) :: SCtx(ctx.variables.last) :: rest)
      )((vd, ti) => h.HOMB(f, STerm(state.markAsUnknown(OMV(state.getUnknown))) :: SCtx(vd) :: STerm(ti) :: Nil))
    case STerm(v1@OMV(_)) :: STerm(v2@OMV(_)) :: SCtx(ctx) :: rest if ctx.nonEmpty && state.isUnknown(v1) && state.isUnknown(v2) =>
      ctx.variables.init.foldRight(
        h.HOMB(f, STerm(v1) :: STerm(v2) :: SCtx(ctx.variables.last) :: rest)
      )((vd, ti) => h.HOMB(f, STerm(state.markAsUnknown(OMV(state.getUnknown))) :: STerm(state.markAsUnknown(OMV(state.getUnknown))) :: SCtx(vd) :: STerm(ti) :: Nil))
    case STerm(t) :: SCtx(ctx) :: rest if ctx.nonEmpty =>
      ctx.variables.init.foldRight(
        h.HOMB(f, STerm(t) :: SCtx(ctx.variables.last) :: rest)
      )((vd, ti) => h.HOMB(f, STerm(t) :: SCtx(vd) :: STerm(ti) :: Nil))
    case SCtx(ctx) :: rest if ctx.nonEmpty =>
      ctx.variables.init.foldRight(
        h.HOMB(f,SCtx(ctx.variables.last) :: rest)
      )((vd,t) => h.HOMB(f,SCtx(vd) :: STerm(t) :: Nil))
    case _ =>
      h.HOMB(f,args)
  }

  def doBinr(h : Option[SHTMLHoas.HoasRule], f:Term,args:List[Term])(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode) = args match {
    case IsSeq(Nil,SHTML.flatseq(ls),rest) if ls.nonEmpty && ls.length + rest.length >= 2  =>
      val ils = ls ::: rest
      ils.init.foldRight(ils.last){case (acc,b) => SHTMLHoas.OmaSpine(h,f,List(acc,b))}
    case IsSeq(impls, SHTML.flatseq(ls), rest) if ls.nonEmpty && ls.length + rest.length >= 2 && impls.forall(x => x.isInstanceOf[OMV] && state.isUnknown(x.asInstanceOf[OMV])) =>
      val ils = ls ::: rest
      ils.init.foldRight(ils.last) { case (acc, b) => SHTMLHoas.OmaSpine(h, f, impls ::: List(acc, b)) }
    case IsSeq(Nil,OMV(v),rest) =>
      val fcont = self.getVariableContext
      val x = Context.pickFresh(fcont, LocalName("foldrightx"))._1
      val y = Context.pickFresh(fcont, LocalName("foldrighty"))._1
      val tp = fcont.findLast(_.name == v) match {
        case Some(v) if v.tp.isDefined =>
          v.tp match {
            case Some(SHTML.flatseq.tp(t)) => t
            case _ =>
              val vn = state.getUnknownTp
              state.markAsUnknown(OMV(vn))
          }
        case _ =>
          val vn = state.getUnknownTp
          state.markAsUnknown(OMV(vn))
      }
      val ret = if (rest.isEmpty) SHTML.seqfoldright(SHTML.seqlast(OMV(v)), SHTML.seqinit(OMV(v)), x, tp, y, tp, SHTMLHoas.OmaSpine(h, f, List(OMV(x),OMV(y))))
      else SHTML.seqfoldright(SHTMLHoas.OmaSpine(h,f,SHTML.seqlast(OMV(v)) :: rest), SHTML.seqinit(OMV(v)), x, tp, y, tp, SHTMLHoas.OmaSpine(h,f, List(OMV(x), OMV(y))))
      ret
    case _ =>
      SHTMLHoas.OmaSpine(h, f, args)
  }

  def doConj(h: Option[SHTMLHoas.HoasRule], f: Term, args: List[Term])(implicit state: SHTMLState[SHTMLNode], self: SHTMLNode) = args match {
    case IsSeq(pre, SHTML.flatseq(a :: b :: Nil), Nil) =>
      SHTMLHoas.OmaSpine(h,f,pre ::: List(a, b))
    case IsSeq(pre, SHTML.flatseq(a :: Nil), List(b)) =>
      SHTMLHoas.OmaSpine(h,f, pre ::: List(a, b))
    case IsSeq(pre, SHTML.flatseq(ls), post) if ls.length > 2 =>
      var conj : Option[Constant] = None
      self.getRuleContext.getIncludes.foreach{i =>
        state.server.ctrl.globalLookup.forDeclarationsInScope(OMMOD(i)){
          case (_,_,c : Constant) if c.rl.map(r => r.split(' ').toList).getOrElse(Nil).contains("conjunction") =>
            conj = Some(c)
          case _ =>
        }
      }
      conj match {
        case Some(c) =>
          def dohead(t1: Term,t2:Term) = SHTMLHoas.OmaSpine(h,f,pre ::: List(t1,t2) ::: post)
          def doconj(t1 : Term,t2:Term) = SHTMLHoas.OmaSpine(h,OMS(c.path),List(t1,t2))
          ls.tail.tail.foldLeft((dohead(ls.head,ls.tail.head),ls.tail.head))((b,t) =>
            (doconj(b._1,dohead(b._2,t)),t)
          )._1
        case _ =>
          SHTMLHoas.OmaSpine(h,f,args)
      }
    case _ => SHTMLHoas.OmaSpine(h,f,args)
  }
}