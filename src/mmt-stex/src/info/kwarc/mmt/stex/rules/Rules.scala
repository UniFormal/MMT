package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Conversions.localName2OMV
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardString}
import info.kwarc.mmt.stex.xhtml.SemanticState
import info.kwarc.mmt.stex.{STeX, rules}

object Rules {
  import info.kwarc.mmt.api.objects.Conversions._
  class Pattern(original : Term) {
    private val (variablesI,bodyI) = getBoundVars(original)
    val body = bodyI
    val variables = variablesI.filterNot(v => original.freeVars.contains(v._1))
    val arity = variables.length
    def apply(args : List[Term]) = if (args.length == arity) {
      body ^? Substitution(variables.indices.map(i => variables(i)._1 / args(i)):_*)
    } else {
      ???
    }

    // TODO
    private def recurse(tmmatch : Term,tmorig : Term)(cont : Context) : Option[List[(LocalName,Term)]] = (tmmatch,tmorig) match {
      case (OMID(s1),OMID(s2)) if s1 == s2 => Some(Nil)
      case (STeX.flatseq(ls),OMV(x)) if variables.exists(_._1 ==x) =>
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
      recurse(tm,body)(Context.empty) /* match {
        case Some(ls) =>
          val dist = ls.distinct
          val vns = dist.map(_._1)
          if (vns == vns.distinct) Some(variables.map(v => ls.find(_._1 == v._1).get)) else None
        case _ => None
      } */
    }
  }
  def getBoundVars(tm : Term) : (List[(LocalName,Option[Term])],Term) = tm match {
    case STeX.binder(ln,tp,bd) =>
      val ret = getBoundVars(bd)
      ((ln,tp) :: ret._1,ret._2)
    case _ => (Nil,tm)
  }
}

trait UsesPatterns extends SingleTermBasedCheckingRule {
  protected val pattern : Rules.Pattern
  override def applicable(t: Term): Boolean = t match {
    case pattern(_) => true
    case _ => false
  }
}

case class UnivRule(pattern : Rules.Pattern,hhead : GlobalName) extends UniverseRule(hhead) with UsesPatterns {

  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
}

object Universe extends ParametricRule {

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Rules.Pattern(tm)
      val head = pattern.body.head match {
        case Some(gn : GlobalName) => gn
        case _ => STeX.informal.sym
      }
      RuleSet(UnivRule(pattern,head),rules.InhabRule(pattern,head))
    case _ =>
      ???
  }
}

case class InhabRule(pattern : Rules.Pattern,hhead : GlobalName) extends InhabitableRule(hhead) with UsesPatterns {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
}

object Inhabitable extends ParametricRule {

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Rules.Pattern(tm)
      val head = pattern.body.head match {
        case Some(gn : GlobalName) => gn
        case _ => STeX.informal.sym
      }
      rules.InhabRule(pattern,head)
    case _ =>
      ???
  }
}

trait HTMLTermRule extends Rule {
  def apply(tm : Term)(implicit state : SemanticState) : Option[Term]
}

object ImplicitBindRule extends HTMLTermRule {
  override def apply(tm: Term)(implicit state: SemanticState): Option[Term] = tm match {
    /*case STeX.implicit_binder(ln,_,bd) =>
      val ret = apply(bd).getOrElse(bd)
      val traverser = new StatelessTraverser {
        override def traverse(t: Term)(implicit con: Context, ss: State): Term = t match {
          case o@OMV(`ln`) =>
            state.markAsUnknown(o)
          case _ => Traverser(this,t)
        }
      }
      Some(traverser(ret,()))*/
    case _ => None
  }
}

object MiMoVariableRule extends HTMLTermRule {
  override def apply(tm: Term)(implicit state: SemanticState): Option[Term] = tm match {
    case STeX.informal(n) if n.label == "mi" && n.child.length == 1 =>
      Some(OMV(n.child.head.toString()))
    case _ => None
  }
}

object ParenthesisRule extends HTMLTermRule {
  val open = new {
    def unapply(tm : Term) = tm match {
      case STeX.informal(o) if o.label == "mo" && o.attribute("class").exists(_.exists(_.toString() == "opening")) &&
        o.child.length == 1 && o.child.head.toString() == "(" => Some(true)
      case _ => None
    }
  }
  val close = new {
    def unapply(tm : Term) = tm match {
      case STeX.informal(o) if o.label == "mo" && o.attribute("class").exists(_.exists(_.toString() == "closing")) &&
        o.child.length == 1 && o.child.head.toString() == ")" => Some(true)
      case _ => None
    }
  }
  override def apply(tm: Term)(implicit state: SemanticState): Option[Term] = tm match {
    case STeX.informal.op("mrow",List(open(_),t,close(_))) => Some(t)
    case _ => None
  }
}

trait BindingRule extends Rule {
  def apply(tm : Term,assoc:Boolean)(implicit state : SemanticState) : Option[Context]
}

object StringLiterals extends RepresentedRealizedType(OMS(STeX.string),StandardString)

object InformalBindingRule extends BindingRule {
  def apply(tm : Term,assoc:Boolean)(implicit state : SemanticState) : Option[Context] = tm match {
    case OMV(x) =>
      val vd = state.getVariableContext.find(_.name == x).getOrElse {
        val v = state.markAsUnknown(OMV(state.getUnknownTp))
        VarDecl(x,tp=v)
      }
      vd.metadata.update(STeX.meta_notation,tm)
      Some(Context(vd))
    case _ => None
  }
}

case class BinderRule(pattern : Rules.Pattern,head : GlobalName,tp : Option[LocalName]) extends BindingRule with UsesPatterns {
  def apply(tm : Term,assoc:Boolean)(implicit state : SemanticState) : Option[Context] = tm match {
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
      }.map{vd => vd.metadata.update(STeX.meta_notation,tm);vd})
    case _ =>
      None
  }
}

object TypingLike extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    if (args.length != 1 && args.length != 2) throw ParseError("one or two arguments expected")
    val (pattern,ln) = args match {
      case List(h,OMV(n)) =>
        (new Rules.Pattern(h),Some(n))
      case List(h,STeX.binder(n,_,OMV(m))) if n == m =>
        (new Rules.Pattern(h),Some(n))
      case List(h) =>
        (new Rules.Pattern(h),None)
      case _ =>
        throw ParseError("second argument needs to be a variable")
    }
    val head = pattern.body.head match {
      case Some(gn : GlobalName) => gn
      case _ => STeX.informal.sym
    }
    BinderRule(pattern,head,ln)
  }
}

object NotationRules extends RuleSet {
  object NInhabitableRule extends InhabitableRule(STeX.notation.tp.sym) {
    override def applicable(t: Term): Boolean = t match {
      case STeX.notation.tp(_,_) => true
      case _ => false
    }
    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] =
      if(applicable(term)) Some(true) else None
  }
  object NTypingRule extends TypingRule(STeX.notation.tp.sym) {
    override def applicable(t: Term): Boolean = NInhabitableRule.applicable(t)

    override def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = (tm,tp) match {
      case (STeX.notation(_,_,_),STeX.notation.tp(_,_)) => Some(true)
      case _ => None
    }
  }
  override def getAll: Iterable[Rule] = Seq(NInhabitableRule,NTypingRule)
}

/*
object Realize extends ParametricRule {

  //TODO checks are called even when an .omdoc file is read
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    if (args.length != 2) throw ParseError("two arguments expected")
    val List(syn,sem) = args
    val mp = sem match {
      case OMMOD(mp) => mp
      case _ =>  throw ParseError("semantic element must be identifier")
    }
    val obj = controller.backend.loadObjectO(mp).getOrElse {
      throw ParseError("semantic object not found")
    }
    obj match {
      case st: SemanticType =>
        RealizedType(syn,st)
      case semVal: SemanticValue =>
        val synP = syn match {
          case OMS(p) => p
          case _ => throw ParseError("realized value must be an identifier")
        }
        val synTp = controller.globalLookup.getO(synP) match {
          case Some(c: Constant) => c.tp.getOrElse {
            throw ParseError("type not present or not fully checked")
          }
          case _ => throw ParseError("realized operator must be a constant")
        }
        new RealizedValue(synP, synTp, semVal)
      case semOp: SemanticOperator =>
        // TODO
        throw ParseError("semantic operator not yet implemented")
      case _ => throw ParseError("objects exists but is not a semantic type or operator")
    }
  }
}

object SeqTypeInhabitable extends InhabitableRule(STeX.flatseq.tp.sym) {
  override def applicable(t: Term): Boolean = t match {
    case OMA(OMS(STeX.flatseq.tp.sym),List(_)) => true
    case _ => false
  }

  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val OMA(OMS(STeX.flatseq.tp.sym),List(tp)) = term
    Some(solver.check(info.kwarc.mmt.api.objects.Inhabitable(stack,tp)))
  }
}

case class FunctionSpaceInhabitable(hhead : GlobalName) extends InhabitableRule(hhead) {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
    case OMA(OMS(`hhead`),List(STeX.flatseq(doms),codom)) =>
      (codom :: doms).foreach {t =>
        solver.check(info.kwarc.mmt.api.objects.Inhabitable(stack,t))
      }
      Some(true)
    case _ =>
      None
  }
}

object FunctionSpaceLike extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head)) =>
      FunctionSpaceInhabitable(head)
    case _ =>
      throw ParseError("one OMID argument expected")
  }
}

object AbbreviationRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),tm) =>
      new AbbrevRule(head,tm)
    case _ =>
      throw ParseError("one OMID and one arbitrary argument expected")
  }
}
*/
/*


case class LetBinderRule(pattern : Rules.Pattern,head : GlobalName,df : Option[LocalName]) extends BindingRule with UsesPatterns {
  def apply(tm : Term)(implicit state : SemanticState) : Option[Context] = tm match {
    case pattern(ls) =>
      val (vars, dfO) = df match {
        case Some(dfn) =>
          val p = ls.find(_._1 == dfn).getOrElse {
            return None
          }
          (ls.filterNot(_ == p), Some(p._2))
        case _ => (ls,None)
      }
      Some(vars.foldLeft(Context.empty) {
        case (ctx, (_, OMV(ln))) =>
          dfO match {
            case Some(t) => ctx ++ VarDecl(ln,None,None,Some(t),None)
            case _ => ctx ++ VarDecl(ln)
          }
        case (ctx, (_, ot)) =>
          val nc = state.makeBinder(ot).variables.map {
            case vd if vd.df.isEmpty =>
              dfO match {
                case Some(t) => VarDecl(vd.name,None,vd.tp,Some(t),None)
                case _ => vd
              }
            case vd => vd
          }
          ctx ++ nc
      }.map{vd => vd.metadata.update(STeX.meta_notation,tm);vd})
    case _ =>
      None
  }
}

object LetBinder extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    if (args.length != 1 && args.length != 2) throw ParseError("one or two arguments expected")
    val (pattern,ln) = args match {
      case List(h,OMV(n)) =>
        (new Rules.Pattern(h),Some(n))
      case List(h,STeX.universal_quantifier(n,_,OMV(m))) if n == m =>
        (new Rules.Pattern(h),Some(n))
      case List(h) =>
        (new Rules.Pattern(h),None)
      case _ =>
        throw ParseError("second argument needs to be a variable")
    }
    val head = pattern.body.head match {
      case Some(gn : GlobalName) => gn
      case _ => STeX.informal.sym
    }
    LetBinderRule(pattern,head,ln)
  }
}

 */