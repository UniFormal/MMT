package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.api._
import checking._
import objects._
import ontology._
import uom._
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.MitM._
import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.LMFDB.LMFDB
import info.kwarc.mmt.odk.{Plugin, StringLiterals}

import scala.util.{Failure, Try}

/** provides computation via all available [[VRESystem]]s */
class MitMComputation(controller: frontend.Controller) extends Logger {
  val logPrefix = "mitm"
  val report: Report = controller.report

  /** simplifies a given term using all known VREs, using special additional rules */
  def simplify(tm : Term, conO : Option[Context])(implicit trace: MitMComputationTrace): Term = {
    //TODO delete this, if databases can supply global context, it should be done generically
    val con1 = conO.getOrElse {Context.empty}
    val con = con1 ++ LMFDB.databases.foldLeft(Context(MitM.mathpath))((c,p) => c ++ Context(p))
    val rs = RuleSet.collectRules(controller,con)
    rs.add(systemRule(con))
    rs.add(queryRule)
    controller.simplifier(tm,SimplificationUnit(con, true,true, true), rs)
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

        // parse the query or bail out
        val parsed = Try(Query.parse(q)(queryexts,controller.relman)) match {
          case util.Success(t: Query) => t
          case util.Failure(t: Throwable) => {
            log(GeneralError("throwable while trying to translate mitm query").setCausedBy(t).toStringLong)
            return Simplifiability.NoRecurse
          }
        }

        // evaluate the query or bail out
        Try(controller.evaluator(parsed)) match {
          case util.Success(qr: QueryResult) => Simplify(resultToTerm(qr))
          case util.Failure(t: Throwable) => {
            log(GeneralError("throwable while trying to evaluate mitm query").setCausedBy(t).toStringLong)
            Simplifiability.NoRecurse
          }
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
      case _ => throw ImplementationError("invalid result")
    }
  }

  /** a rule used to evaluate code within systems */
  private def systemRule(con : Context)(implicit trace: MitMComputationTrace) : ComputationRule = new ComputationRule(MitMSystems.evalSymbol) {
    override def alternativeHeads: List[GlobalName] = List(Apply.path)

    override def applicable(tm: Term): Boolean = tm match {
      case OMA(OMS(MitMSystems.evalSymbol),List(OMS(_),_)) | ApplySpine(OMS(MitMSystems.evalSymbol),List(_,OMS(_),_)) => true
      case _ => false
    }

    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      val (sys, subtm) = tm match {
        case OMA(OMS(MitMSystems.evalSymbol),List(OMS(s),t)) => (s,t) // official
        case ApplySpine(OMS(MitMSystems.evalSymbol),List(_,OMS(s),t)) => (s,t)
        case _ => return Simplifiability.NoRecurse
      }
      Simplify(callVRE(dropImplicits(simplify(subtm,Some(con))),sys))
    }

    def dropImplicits(tm : Term) = {
      //println("Term in: " + tm.toStr(true))
      val ret = dropTrav(tm,())
      //println("Term out: " + ret.toStr(true))
      ret
    }
    val dropTrav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine(OMS(p),args) => controller.getO(p) match {
          case Some(c : Constant) if c.not.isDefined =>
            val imps = c.not.get.arity.flatImplicitArguments(args.length).map(_.number -1)
            val nargs = args.zipWithIndex.collect {
              case (it,i) if !imps.contains(i) => it
            }
            ApplySpine(OMS(p),nargs:_*)
          case _ => Traverser(this,t)
        }
        case _ => Traverser(this,t)
      }
    }
  }

  /** calls a single Virtual Research Environment */
  private def callVRE(t : Term, system : GlobalName)(implicit trace: MitMComputationTrace): Term = {
    val systems = controller.extman.get(classOf[VRESystem])
    systems.find(_.sym == system) match {
      case Some(s) =>
        s.call(t)
      case _ => ???
    }
  }
}


/** used for tracing computation run by [[VREComputation]] */
class MitMComputationTrace(present: Option[Term => String]) {
  var steps: List[MitMTracePart] = Nil
  def +=(s: MitMTracePart): Unit = {
    present foreach {p => 
      println(s.toString(p) + "\n")
    }
    steps ::= s
  }
  
  def toString(present: Term => String) = steps.reverseMap(_.toString(present)).mkString("\n\n")
}

/** dummy for ignoring the trace */
object NoTrace extends MitMComputationTrace(None)

/** used in [[MitMComputationTrace]] */
abstract class MitMTracePart {
  def toString(present: Term => String): String
}

case class MitMFailure(error: Error) extends MitMTracePart {
  def toString(present: Term => String) = {
    error.toStringLong
  }
}

/** adds functions for conveniently tracing computation steps */
abstract class MitMComputationStep extends MitMTracePart {
  def header: String
  def terms: List[(String, Any)]
  def toString(present: Term => String) = {
    val termsS = terms map {
      case (s,t: Term) => s + ": " + present(t)
      case (s,t) => s + ": " + t 
    }
    header + "\n" + termsS.mkString("\n")
  }
}

case class AlignmentFromMitMStep(system: String, mitm: Term, extern: Term) extends MitMComputationStep {
  val header = "alignment"
  val terms = List("MitM" -> mitm, system -> extern)
}

case class AlignmentToMitMStep(system: String, extern: Term, mitm: Term) extends MitMComputationStep {
  val header = "alignment"
  val terms = List(system -> extern, "MitM" -> mitm)
}

case class VRECallDetected(system: String, term: Term) extends MitMComputationStep {
  val header = "processing MitM call to system " + system
  val terms = List("term" -> term)
}

case class InitialTerm(term: Term) extends MitMComputationStep {
  val header = "start"
  val terms = List("initial term" -> term)
}