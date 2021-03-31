package info.kwarc.mmt.stex.features

import info.kwarc.mmt.api.checking.{History, InhabitableRule, Solver, UniverseRule}
import info.kwarc.mmt.api.{GlobalName, LocalName, ParametricRule, ParseError, Rule, RuleSet}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{ComplexTheory, Context, OMA, OMID, OMMOD, OMS, OMV, Stack, Substitution, Term}
import info.kwarc.mmt.api.symbols.{Constant, RuleConstant}
import info.kwarc.mmt.api.uom.{RealizedType, RealizedValue, SemanticOperator, SemanticType, SemanticValue}
import info.kwarc.mmt.lf.FunType
import info.kwarc.mmt.stex.STeX

object Rules {
  import info.kwarc.mmt.api.objects.Conversions._
  class Pattern(original : Term) {
    private val (variablesI,bodyI) = getBoundVars(original)
    val body = bodyI
    val variables = variablesI.filter(v => original.freeVars.contains(v._1))
    val arity = variables.length
    def apply(args : List[Term]) = if (args.length == arity) {
      body ^? Substitution(variables.indices.map(i => variables(i)._1 / args(i)):_*)
    } else {
      ???
    }

    // TODO
    private def recurse(tmmatch : Term,tmorig : Term)(cont : Context) : Option[List[(LocalName,Term)]] = (tmmatch,tmorig) match {
      case (OMID(s1),OMID(s2)) if s1 == s2 => Some(Nil)
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
    }
    def unapply(tm : Term) = {
      recurse(tm,original)(Context.empty) match {
        case Some(ls) =>
          val dist = ls.distinct
          val vns = dist.map(_._1)
          if (vns == vns.distinct) Some(variables.map(v => ls.find(_._1 == v._1).get)) else None
        case _ => None
      }
    }
  }
  def getBoundVars(tm : Term) : (List[(LocalName,Option[Term])],Term) = tm match {
    case STeX.universal_quantifier(ln,tp,bd) =>
      val ret = getBoundVars(bd)
      ((ln,tp) :: ret._1,ret._2)
    case _ => (Nil,tm)
  }
}

object Universe extends ParametricRule {
  case class UnivRule(pattern : Rules.Pattern,hhead : GlobalName) extends UniverseRule(hhead) {
    override def applicable(t: Term): Boolean = t match {
      case pattern(_) => true
      case _ => false
    }

    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
  }
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Rules.Pattern(tm)
      val head = pattern.body.head match {
        case Some(gn : GlobalName) => gn
        case _ => STeX.informal.sym
      }
      UnivRule(pattern,head)
    case _ =>
      ???
  }
}

object Inhabitable extends ParametricRule {
  case class InhabRule(pattern : Rules.Pattern,hhead : GlobalName) extends InhabitableRule(hhead) {
    override def applicable(t: Term): Boolean = t match {
      case pattern(_) => true
      case _ => false
    }

    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
  }
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Rules.Pattern(tm)
      val head = pattern.body.head match {
        case Some(gn : GlobalName) => gn
        case _ => STeX.informal.sym
      }
      InhabRule(pattern,head)
    case _ =>
      ???
  }
}

object Realize extends ParametricRule {
/*
  /** finds the semantic type that was previously declared to realized a syntactic type */
  private def getSemanticType(controller: Controller, home: Term, synTp: Term): SemanticType = {
    controller.globalLookup.forDeclarationsInScope(home) {
      case (_,m,rc: RuleConstant) => rc.df match {
        // TODO translate rt along m
        case Some(rt: RealizedType) if rt.synType == synTp => return rt.semType
        case _ =>
      }
      case _ =>
    }
    throw ParseError("no realized type known: " + synTp)
  }
  */

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
