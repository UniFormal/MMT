import info.kwarc.mmt.api.refactoring.ViewFinder
import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.got.GraphOptimizationTool
import info.kwarc.mmt.jedit.MMTOptimizationAnnotationReader

object RunMichael extends MagicTest {

  def run : Unit = {
    viewfinder
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
      val from = Path.parseM("http://cds.omdoc.org/testcases?BeautifulSets",NamespaceMap.empty)
      // val from = Path.parseM("http://cds.omdoc.org/testcases?CommTest",NamespaceMap.empty)
      // val from = Path.parseM("http://cds.omdoc.org/testcases?PVSTest",NamespaceMap.empty)

      // val to = "PVS/NASA"
      // val to = "HOLLight/basic"
      val to = "MitM/smglom"
      // val to = "PVS/Prelude"

      val vf = new ViewFinder
      controller.extman.addExtension(vf,List(
        "MitM/smglom"
        //,"HOLLight/basic"
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
}
