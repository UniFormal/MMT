package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.api._
import checking._
import objects._
import ontology._
import uom._

import info.kwarc.mmt.lf.{Apply, ApplySpine}

import info.kwarc.mmt.MitM._

import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.{Plugin, StringLiterals}

import scala.util.Try

/** provides computation via external services */
trait VREComputation { this: Plugin =>
  
  /** simplifies a given term using all known VREs, using special additional rules */
  def simplify(tm : Term, conO : Option[Context]): Term = {
    val con = conO.getOrElse {
      controller.getTheory(MitM.mathpath).getInnerContext
    }
    val rs = RuleSet.collectRules(controller,con)
    rs.add(systemRule(con))
    rs.add(queryRule)
    controller.simplifier.apply(tm,con,rs,expDef=true)
  }

  /** a rule used to evaluate MMT queries */
  private val queryRule: ComputationRule = new ComputationRule(MitMSystems.querysym) {
    override def alternativeHeads: List[GlobalName] = List(Apply.path)

    override def applicable(tm: Term): Boolean = IsQuery.unapply(tm).isDefined

    private object IsQuery {
      def unapply(tm: Term) = tm match {
        case OMA(OMS(MitMSystems.querysym), List(r)) => Some(r)  // official
        case ApplySpine(OMS(MitMSystems.querysym), List(r)) => Some(r) // shouldn't happen
        case _ => None
      }
    }

    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
      case IsQuery(q) =>
        val queryexts = controller.extman.get(classOf[QueryFunctionExtension])
        val ret = Try(controller.evaluator(Query.parse(q)(queryexts,controller.relman))).toOption
        ret match {
          case Some(qr) => Simplify(resultToTerm(qr))
          case _ => Simplifiability.NoRecurse
        }
      case _ => Simplifiability.NoRecurse
    }

    def resultToTerm(qr : QueryResult) : Term = qr match {
      case SetResult(s) => LFList(s.toList.map(resultToTerm))
      case ElemResult(ls :: Nil) => ls match {
        case t : Term => t
        case p : ContentPath => OMID(p)
        case s : StringValue => StringLiterals(s.string)
        case _ => ???
      }
    }

  }

  /** a rule used to evaluate code within systems */
  private def systemRule(con : Context) : ComputationRule = new ComputationRule(MitMSystems.evalSymbol) {
    override def alternativeHeads: List[GlobalName] = List(Apply.path)

    override def applicable(tm: Term): Boolean = tm match {
      case OMA(OMS(MitMSystems.evalSymbol),List(OMS(_),_)) | ApplySpine(OMS(MitMSystems.evalSymbol),List(OMS(_),_)) => true
      case _ => false
    }

    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      val (sys, subtm) = tm match {
        case OMA(OMS(MitMSystems.evalSymbol),List(OMS(s),t)) => (s,t) // official
        case ApplySpine(OMS(MitMSystems.evalSymbol),List(OMS(s),t)) => (s,t)
        case _ => return Simplifiability.NoRecurse
      }
      Simplify(callVRE(simplify(subtm,Some(con)),sys))
    }
  }

  /** calls a single Virtual Research Environment */
  private def callVRE(t : Term, system : GlobalName): Term = {
    val systems = controller.extman.get(classOf[VRESystem])
    systems.find(_.sym == system) match {
      case Some(s) =>
        s.call(t)
      case _ => ???
    }
  }
}
