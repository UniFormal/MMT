package info.kwarc.mmt.odk



import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.MitM.Server._
import info.kwarc.mmt.MitM.VRESystem._
import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.frontend._
import actions._
import info.kwarc.mmt.odk.OpenMath.CodingServer

/** the plugin used for ODK */
// FR this used to mix in MitMComputation for no apparent reason and Config.Actions for bad reasons
class Plugin extends ChangeListener {
  override val logPrefix = "odk"

  val theory: MPath = MitM.mathpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")

  override def start(args: List[String]): Unit = {
    // load the systems
    // FR: done explicitly below now
    // controller.extman.addExtension(MitMConfigActionCompanion)

    // MitM systems
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
    controller.extman.addExtension(new Sage.Plugin)
    controller.extman.addExtension(new Singular.Plugin)
    
    controller.extman.addExtension(Warmup)
    controller.extman.addExtension(WarmupCompanion)

    // custom servers
    controller.extman.addExtension(new MitMComputationServer)
    controller.extman.addExtension(new activecomp.Plugin)
    controller.extman.addExtension(new ODKGraph)
    controller.extman.addExtension(new CodingServer)

    // typing magic
    controller.extman.addExtension(new UniverseInference)
    controller.extman.addExtension(new SubtypeGenerator)

    // initialization
    controller.extman.addExtension(MitM.preproc)
  }
}

/** action for caching in MitM systems */
case object Warmup extends Action with MitMExtension {
  def apply(): Unit = {
    controller.extman.get(classOf[VRESystem]).foreach({ v =>
      log(s"warming up ${v.id}")
      v.warmup()
    })
  }
  
  def toParseString = "mitm warmup"
}

object WarmupCompanion extends ObjectActionCompanion(Warmup, "warms up external MitM systems", "mitm warmup") with MitMExtension
