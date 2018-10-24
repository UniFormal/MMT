package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.MitM.Config.MitMSystemLocation
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.refactoring._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.Plugin

import scala.util.Try

/** base class for systems that provide external computation */
abstract class VRESystem(val id : String, val sym : GlobalName) extends QueryExtension(id) {
  override val logPrefix: String = id

  /** the odk plugin, so that we can use it */
  protected lazy val odkPlugin: Plugin = controller.extman.get(classOf[Plugin]).head

  /** loads up the cache used by this VRESystem */
  def warmup(): Unit

  /** calls the system using a given term */
  def call(t : Term) : Term

  /** evaluates a given query inside of this system */
  def evaluate(q: Query, e: QueryEvaluator)(implicit substitution: QuerySubstitution): scala.collection.mutable.HashSet[List[BaseType]] = {
    // evaluate the query normally
    val result = e.evalSet(q)

    // and return the map
    result.map({
      case List(t: Term) => List(call(t))
      case _ => throw ImplementationError("Failed to evaluate Query with VRE")
    })
  }
}

case class VREComputationStep(system: String, mitmIn: Term, systemIn: Term, systemOut: Term, mitmOut: Term) {
  def toString(present: Term => String) = {
    val terms = List("MitM input" -> mitmIn, system + " input" -> systemIn, system + " output" -> systemOut, "MitM output" -> mitmOut)
    s"MitM computation via system $system\n" +
    (terms.map{case (s,t) => s + ": " + present(t)}).mkString("\n")
  }
}

/** main implementation of [[VRESystem]] */
abstract class VREWithAlignmentAndSCSCP(id : String, sym : GlobalName, val head: OMSymbol, archiveId : String)
         extends VRESystem(id, sym) with UsesAlignments with UsesSCSCP {
  def location: MitMSystemLocation = odkPlugin.config.get(id)

  lazy val archive: Archive = controller.backend.getArchive(archiveId).getOrElse(throw GeneralError(s"Missing archive $archiveId"))
  
  private def present(t: Term) = controller.presenter.asString(t)
  
  def call(t : Term): Term = {
    val tS = translateToSystem(t)
    val tSC = scscpcall(tS)
    val tSCM = translateToMitM(tSC)
    val trace = VREComputationStep(id, t, tS, tSC, tSCM)
    println(trace.toString(_.toString))
    tSCM
  }
}
