import info.kwarc.mmt._
import MitM._
import odk._
import Sage._
import GAP._
import Singular._
import info.kwarc.mmt.MitM.VRESystem.MitMComputation
import info.kwarc.mmt.api.objects.{OMA, OMS}

object JanesUseCase extends MagicTest("lmfdb", "mitm", "scscp") {
  override val gotoshell = false
  def run {
    // turn on scscp on localhost:26134:
    hl("scscp on 26134")
    
    val gap = controller.extman.get(classOf[GAPSystem]).head
    val sage = controller.extman.get(classOf[SageSystem]).head
    val singular = controller.extman.get(classOf[SingularSystem]).head
    
    implicit val trace = new VRESystem.MitMComputationTrace

    val group = OMA(OMS(MitM.dihedralGroup),IntegerLiterals.of(4):: Nil)
    val ring = OMS(MitM.rationalRing)
    val poly = OMA(OMS(MitM.polycons),List(ring,MitM.Monomial(List(("x1",1)),3,ring),MitM.Monomial(List(("x2",1)),2,ring)))
    val o = OMA(OMS(MitM.polyOrbit),group :: poly :: Nil)

    gap.call(o)
    val mitm = new MitMComputation(controller)
    mitm.simplify(OMA(OMS(MitMSystems.evalSymbol),OMS(MitMSystems.gapsym):: o :: Nil),None)
    
    println(trace.toString(t => controller.presenter.asString(t)))
    sys.exit
  }
}
