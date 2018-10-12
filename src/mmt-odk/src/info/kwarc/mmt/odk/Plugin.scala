package info.kwarc.mmt.odk

import info.kwarc.mmt.MitM.Config.{Actions, MitMConfig}
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.MitM.Server.Server
import info.kwarc.mmt.MitM.VRESystem.Rules
import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.odk.OpenMath.CodingServer

/** the plugin used for ODK */
class Plugin extends ChangeListener with Rules with Actions {
  val theory: MPath = MitM.mathpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")

  override def start(args: List[String]) {

    // the three other systems that we know of
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
    controller.extman.addExtension(new Sage.Plugin)
    controller.extman.addExtension(new Singular.Plugin)

    // custom servers
    controller.extman.addExtension(new Server)
    controller.extman.addExtension(new activecomp.Plugin)
    controller.extman.addExtension(new ODKGraph)
    controller.extman.addExtension(new CodingServer)

    // typing magic
    controller.extman.addExtension(new UniverseInference)
    controller.extman.addExtension(new SubtypeGenerator)

    // initialization
    controller.extman.addExtension(MitM.preproc)
  }

  var config: MitMConfig = MitMConfig.default
}
