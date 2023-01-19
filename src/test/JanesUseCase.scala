import info.kwarc.mmt.api._
import objects._
import utils._
import info.kwarc.mmt.MitM._
import info.kwarc.mmt.odk._
import Sage._
import GAP._
import Graphtester.controller
import Singular._
import info.kwarc.mmt.MitM.VRESystem._
import info.kwarc.mmt.api.objects.{OMA, OMS}
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.web.JSONBasedGraphServer
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.odk.OpenMath._

object JanesUseCase extends MagicTest("lmfdb", "mitm", "scscp","checkalign","translator") {
  def newTrace = new VRESystem.MitMComputationTrace(Some(t => MitM.present(t, s => controller.presenter.asString(s))))

  override def doFirst(): Unit = {
    super.doFirst()
    // Copied here because these lines were removed from MagicTest.
    // Please reevaluate if they are necessary. If in doubt, leave them. They are just slow.)
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine(("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath).trim)
    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
  }

  // override val gotoshell = false
  def run(): Unit = {
    // turn on scscp on localhost:26134:
    hl("scscp on 26134")
    //hl("extension info.kwarc.mmt.api.ontology.AddAlignments /home/jazzpirate/work/Stuff/AlignmentsPublic/odk")
    
    val sage = controller.extman.get(classOf[SageSystem]).head
    val gap = controller.extman.get(classOf[GAPSystem]).head
    val singular = controller.extman.get(classOf[SingularSystem]).head
    

    val group = Apply(OMS(MitM.dihedralGroup),IntegerLiterals(4))
    val ring = OMS(MitM.rationalRing)
    val poly = MitM.MultiPolynomial(ring,List(MitM.Monomial(List(("x1",1)),3,ring),MitM.Monomial(List(("x2",1)),2,ring)))// MitM.multi_polycon(ring,MitM.Monomial(List(("x1",1)),3,ring),MitM.Monomial(List(("x2",1)),2,ring))

    val orbit = ApplySpine(OMS(MitM.polyOrbit),ring,group, poly)

    // ************* test the call to Gap only
    // val gapResult = test(gap, orbit)
      
    // ************* test the call to Singular only
    //val ideal = termFromFile(filename(gap))                         // use the expected Gap result
    //val ideal = gapResult                                             // use the Gap result from this run
    // val groebner = MitM.groebner(ideal)

    // val singularResult = test(singular, groebner)
    
    // ************* test calling Sage only
    //val singularResult = termFromFile(filename(singular))
    //test(sage, singularResult) // Sage just reads the polynomials and send them back
    
    // ************* test the Singular/Gap simplification    
    val mitm = new MitMComputation(controller)
    val trace = newTrace
    val jane = singular(ApplySpine(OMS(MitM.groebner),ring,gap(orbit)))

    val c = controller.get(Path.parseS("http://test.org?Jane?singcall",NamespaceMap.empty)).asInstanceOf[FinalConstant]
    mitm.simplify(c.df.get,None)(trace)
    //trace += InitialTerm(jane)
    //val janeResult = mitm.simplify(jane,None)(trace)
    //test(sage, janeResult)
    
    //printTraceToFiles(trace, "Jane")
  }
  
  def filename(sys: VRESystem) = "JanesUseCase_" + sys.id + "ResultExpected" 
    
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
  
  private def printTraceToFiles(trace: MitMComputationTrace, folder: String): Unit = {
    trace.steps.reverse.zipWithIndex.foreach {case (s,i) => s match {
      case SCSCPSend(sys, t, om) =>
        val prefix = folder + "/" + i + "-scscp-send-to-" + sys
        //termToFile(t, prefix)
        omToFile(om, prefix)
      case SCSCPReceive(sys, om, t) =>
        val prefix = folder + "/" + i + "-scscp-receive-from-" + sys
        //termToFile(t, prefix)
        omToFile(om, prefix)
      case AlignmentFromMitMStep(sys, tM, tS) =>
        val prefix = folder + "/" + i + "-alignment-to-" + sys
        termToFile(tM, prefix)
      case AlignmentToMitMStep(sys, tS, tM) =>
        val prefix = folder + "/" + i + "-alignment-from-" + sys
        termToFile(tM, prefix)
      case InitialTerm(t) =>
        val prefix = folder + "/" + i + "-initial-term"
        termToFile(t, prefix)
      case _ =>
    }}    
  }
    
  private def termFromFile(f: String) = {
    val n = utils.xml.readFile(File(f))
    Obj.parse(n, NamespaceMap.empty).asInstanceOf[Term]
  }

  private def termToFile(t: Term, f: String): Unit = {
    File.write(File(f+".mitm"), t.toNode.toString)
  }
  private def omToFile(om: OMAny, f: String): Unit = {
    File.write(File(f+".openmath"), (new Coding.OMXMLCoding).encode(om).toString)
  }
}
