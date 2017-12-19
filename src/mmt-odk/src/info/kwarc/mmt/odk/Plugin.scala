package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, Solver, SubtypingRule}
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk.SCSCP.Server.MitMServer
import info.kwarc.mmt.odk.Singular.SingularImporter

class Plugin extends frontend.Extension {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
    controller.extman.addExtension(new Sage.Plugin)
    controller.extman.addExtension(new SingularImporter)
    controller.extman.addExtension(new activecomp.Plugin)
    controller.extman.addExtension(new ODKGraph)
    controller.extman.addExtension(new MitMServer)
  }
}

class GAPSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("GAP",GAP.GAP._base,serverurl,port)
class SingularSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Singular",Singular.Singular.dpath,serverurl,port)
class SageSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Sage",Sage.Sage._base,serverurl,port)

object RemoteGAPSystem extends GAPSystem("neptune.eecs.jacobs-university.de")

object MitM {
   val path = DPath(URI("http","mathhub.info") / "MitM" / "Foundation")
}
