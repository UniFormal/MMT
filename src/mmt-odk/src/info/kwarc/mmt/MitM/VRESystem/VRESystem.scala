package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.odk.OpenMath.OMSymbol

/** base class for systems that provide external computation */
abstract class VRESystem(val id : String, val sym : GlobalName) extends QueryExtension(id) {
  override val logPrefix: String = id

  /** the odk plugin, so that we can use it */
  //protected lazy val odkPlugin: Plugin = controller.extman.get(classOf[Plugin]).head

  /** loads up the cache used by this VRESystem */
  def warmup(): Unit

  /** calls the system using a given term */
  def call(t : Term)(implicit trace: MitMComputationTrace) : Term

  /** evaluates a given query inside of this system */
  def evaluate(q: Query, e: QueryEvaluator)(implicit substitution: QuerySubstitution): scala.collection.mutable.HashSet[List[BaseType]] = {
    // evaluate the query normally
    val result = e.evalSet(q)

    // and return the map
    result.map({
      case List(t: Term) => List(call(t)(NoTrace))
      case _ => throw ImplementationError("Failed to evaluate Query with VRE")
    })
  }
}

/** main implementation of [[VRESystem]] */
abstract class VREWithAlignmentAndSCSCP(id : String, sym : GlobalName, headName: GlobalName, archiveId : String)
         extends VRESystem(id, sym) with UsesAlignments with UsesSCSCP {
  // def location: MitMSystemLocation = odkPlugin.config.get(id) // FR this belongs into UsesSCSCP, not here

  val head: OMSymbol = OMSymbol(headName.name.toString, headName.module.name.toString, None, None)

  lazy val archive: Archive = controller.backend.getArchive(archiveId).getOrElse(throw GeneralError(s"Missing archive $archiveId"))
  
  def call(t : Term)(implicit trace: MitMComputationTrace): Term = {
    val tS = translateToSystem(t)
    val step = AlignmentFromMitMStep(id, t, tS)
    trace += step
    val tSC = scscpcall(tS)
    val tSCM = translateToMitM(tSC)
    val step2 = AlignmentToMitMStep(id, tSC, tSCM)
    trace += step2
    tSCM
  }
}
