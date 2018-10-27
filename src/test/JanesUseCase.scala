import info.kwarc.mmt.api._
import objects._
import utils._

import info.kwarc.mmt.MitM._
import info.kwarc.mmt.odk._
import Sage._
import GAP._
import Singular._
import info.kwarc.mmt.MitM.VRESystem._
import info.kwarc.mmt.api.objects.{OMA, OMS}

object JanesUseCase extends MagicTest("lmfdb", "mitm", "scscp", "translator","checkalign") {
  def newTrace = new VRESystem.MitMComputationTrace(true)
  
  // override val gotoshell = false
  def run {
    // turn on scscp on localhost:26134:
    hl("scscp on 26134")
    //hl("extension info.kwarc.mmt.api.ontology.AddAlignments /home/jazzpirate/work/Stuff/AlignmentsPublic/odk")
    
    val sage = controller.extman.get(classOf[SageSystem]).head
    val gap = controller.extman.get(classOf[GAPSystem]).head
    val singular = controller.extman.get(classOf[SingularSystem]).head
    

    val group = MitM.dihedralGroup(IntegerLiterals(4))
    val ring = OMS(MitM.rationalRing)
    val poly = MitM.multi_polycon(ring,MitM.Monomial(List(("x1",1)),3,ring),MitM.Monomial(List(("x2",1)),2,ring))

    val orbit = MitM.polyOrbit(group, poly)

    // ************* test the call to Gap only
    // val gapResult = test(gap, orbit)
      
    // ************* test the call to Singular only
    val ideal = termFromFile(filename(gap))                         // use the expected Gap result
    // val ideal = MitMSystems.evalSymbol(OMS(gap.sym), gapResult)  // use the Gap result from this run
    val groebner = MitM.groebner(ideal)

    // val singularResult = test(singular, groebner)
    
    // ************* test the entire simplification    
    val mitm = new MitMComputation(controller)
    val trace = newTrace
    
    val jane = singular(MitM.groebner(gap(orbit)))
    val janeResult = mitm.simplify(jane,None)(trace)
 
  }
  
  def filename(sys: VRESystem) = "JanesUseCase_" + sys.id + "ResultExpected.om" 
    
  private def test(sys: VRESystem, t: Term, compare: Boolean = false, store: Boolean = false): Term = {
      val trace = newTrace
      val file = filename(sys)
      val computed = sys(trace, t)
      // if result is correct, store it for later tests
      if (store)
         termToFile(computed, file)
      if (compare) {
        val expected = termFromFile(file)
        assert(expected == computed)
      }
      computed
  }
    
  private def termFromFile(f: String) = {
    val n = utils.xml.readFile(File(f))
    Obj.parse(n, NamespaceMap.empty).asInstanceOf[Term]
  }

  private def termToFile(t: Term, f: String) {
    File.write(File(f), t.toNode.toString)
  }
    

}
