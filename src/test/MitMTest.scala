import info.kwarc.mmt._
import MitM.MitM._
import odk._
import Sage._
import GAP._
import Singular._

object MitMTest extends MagicTest("lmfdb", "mitm", "scscp") {
  def run {
    // FR: subsumed by loading ODK plugin, see file Config/Actions.scala
    // load the (default) configuration
    // hl("mitm use")
    
    // turn on scscp on localhost:26134:
    hl("scscp on 26134")
    
    val gap = controller.extman.get(classOf[GAPSystem]).head
    val sage = controller.extman.get(classOf[SageSystem]).head
    val singular = controller.extman.get(classOf[SingularSystem]).head
    
    implicit val trace = new MitM.VRESystem.MitMComputationTrace
    
    gap.call(tt)
    
    println(trace.toString(t => controller.presenter.asString(t)))
    sys.exit
  }
}
