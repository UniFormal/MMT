import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.got.GraphOptimizationTool
import info.kwarc.mmt.jedit.MMTOptimizationAnnotationReader

object RunMichael extends MagicTest {

  def run : Unit = {
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
}
