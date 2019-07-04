import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.MitM.MitM.{eq, implicitProof, logic}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{Context, OMS, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.refactoring.{Intersecter, Moduleadder, Preprocessor, SimpleParameterPreprocessor, ViewFinder, Viewset}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{LocalName, NamespaceMap, Path}
import info.kwarc.mmt.got.GraphOptimizationTool
import info.kwarc.mmt.jedit.MMTOptimizationAnnotationReader
import info.kwarc.mmt.lf.{ApplySpine, LFClassicHOLPreprocessor}

object RunMichael extends MagicTest {

  def run : Unit = {
    intersect
    //viewfinder
  }

  def got : Unit = {
    controller.handleLine("extension info.kwarc.mmt.got.GraphOptimizationTool")
    val got : GraphOptimizationTool = controller.extman.get(classOf[GraphOptimizationTool]).head

    val list = Path.parseM("http://mydomain.org/testarchive/mmt-example?test_other",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_all",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_future",NamespaceMap.empty) :: Nil
    val starttime = System.currentTimeMillis()
    //controller.handleLine("build testarchive got")
    controller.handleLine("extension info.kwarc.mmt.jedit.MMTOptimizationAnnotationReader")
    val or : MMTOptimizationAnnotationReader = controller.extman.get(classOf[MMTOptimizationAnnotationReader]).head
    //println(list.head)
    //println(or(list.head))
    //println(got.toXML(got.findReplacements()))
    //println(got.toXML(got.findReplacements(list, true)))
    //println((System.currentTimeMillis()-starttime)/1000)
  }

  def viewfinder: Unit = {
    // controller.extman.addExtension(HOLLight.preproc)
      // controller.extman.addExtension(PVSTheory.preproc)
      // controller.extman.addExtension(MitM.preproc)
      hl("log+ viewfinder")
      val pvsmonoid = Path.parseM("http://shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library/algebra?monoid",NamespaceMap.empty)
      val mitmmonoid = Path.parseM("http://mathhub.info/Tutorials/Mathematicians?Monoid",NamespaceMap.empty)

      // val from = Path.parseM("http://mathhub.info/MitM/Foundation?RealLiterals",NamespaceMap.empty)
      //val from = Path.parseM("http://cds.omdoc.org/testcases?BeautifulSets",NamespaceMap.empty)
      // val from = Path.parseM("http://cds.omdoc.org/testcases?CommTest",NamespaceMap.empty)
      // val from = Path.parseM("http://cds.omdoc.org/testcases?PVSTest",NamespaceMap.empty)
      val from = Path.parseM("http://mydomain.org/testarchive/mmt-example?addition",NamespaceMap.empty)

      // val to = "PVS/NASA"
      // val to = "HOLLight/basic"
      // val to = "MitM/smglom"
      // val to = "PVS/Prelude"
      val to = "testarchive"

      val eq = logic ? "eq"
      object EliminateImplicits extends Preprocessor {
        val trav = new StatelessTraverser {
          override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
            case ApplySpine(OMS(`implicitProof`),_) => OMS(implicitProof)
            case _ => Traverser(this,t)
          }
        }
        override protected def doTerm(tm: Term): Term = super.doTerm(tm)
      }
      val preproc = (SimpleParameterPreprocessor + info.kwarc.mmt.api.refactoring.DefinitionExpander + EliminateImplicits +
        new LFClassicHOLPreprocessor(
        ded = MitM.ded,
        and = MitM.and,
        not = MitM.not,
        or = Some(MitM.or),
        implies = Some(MitM.implies),
        equiv = Some(MitM.equiv),
        forall = Some(MitM.forall),
        exists = Some(MitM.exists),
        equal = Some(eq)
      )).withKey("testarchive").withKey(logic)
      controller.extman.addExtension(preproc)
      val vf = new ViewFinder
      controller.extman.addExtension(vf,List(
        "testarchive"
        , "MitM/smglom"
        // ,"HOLLight/basic"
        // ,"PVS/Prelude"
        // ,"PVS/NASA"
      ))
      while(!vf.isInitialized) {
        Thread.sleep(500)
      }

      vf.find(from,to).foreach(r => log(r.toString))
      // vf.find(mitmmonoid,to).foreach(r => log(r.toString))
      // vf.find(pvsmonoid,to).foreach(r => log(r.toString))
  }

  def intersect: Unit = {
    val int = new Intersecter
    controller.extman.addExtension(int, Nil)

    val view = controller.get(Path.parseM("http://mydomain.org/testarchive/mmt-example?mono",NamespaceMap.empty)).asInstanceOf[View]
    //val (s1, s2, t1, t2) =
    println(int(view))
    //println(s1)
    //println(s2)
    //println(t1)
    //println(t2)
  }

  def exportMMT: Unit = {
    //controller.handleLine("extension info.kwarc.mmt.api.presentation.MMTSyntaxPresenter")
    controller.extman.addExtension(new MMTSyntaxPresenter(), List())
    val msp = controller.extman.get(classOf[MMTSyntaxPresenter]).head
    val from = Path.parseM("http://cds.omdoc.org/testcases?BeautifulSets",NamespaceMap.empty)
    val th = controller.get(from)
    val sb = new info.kwarc.mmt.api.presentation.StringBuilder()
    msp(th)(sb)
    log(sb.get)
  }

  def moduleadder: Unit = {
    controller.extman.addExtension(new MMTSyntaxPresenter(), List())
    val msp = controller.extman.get(classOf[MMTSyntaxPresenter]).head
    val from = Path.parseS("http://mydomain.org/testarchive/mmt-example?test_base?base_type",NamespaceMap.empty)
    val from2 = Path.parseS("http://mydomain.org/testarchive/mmt-example?test_all?final",NamespaceMap.empty)
    val to = Path.parseM("http://cds.omdoc.org/testcases?BeautifulSets",NamespaceMap.empty)
    val const = controller.getConstant(from).asInstanceOf[FinalConstant]
    val th = Theory.empty(Path.parseD("http://mydomain.org/testarchive/mmt-example",NamespaceMap.empty), LocalName("TH'"), None)
    controller.add(th)
    //controller.getTheory(to)
    Moduleadder(th, List(from, from2), controller)
    val sb = new info.kwarc.mmt.api.presentation.StringBuilder()
    msp(th)(sb)
    log(sb.get)
    log(th.toString)
  }
}
