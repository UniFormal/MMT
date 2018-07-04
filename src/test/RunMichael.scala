import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.got.GraphOptimizationTool

object RunMichael extends MichaelTest {

  def run : Unit = {
    controller.extman.addExtension(new GraphOptimizationTool)
    val got : GraphOptimizationTool = controller.extman.get(classOf[GraphOptimizationTool]).head
    //var path : MPath = Path.parseM("http://mathhub.info/Teaching/KRMT?FOLSyntax",NamespaceMap.empty)
    /*
    var path : MPath = Path.parseM("http://mydomain.org/myarchive/mmt-example?test_united",NamespaceMap.empty)
    var theory : DeclaredTheory = controller.get(path) match {
      case t : DeclaredTheory => t
      case _ => ???
    }
    path = Path.parseM("http://latin.omdoc.org/category_theory/dfol_based?Comp",NamespaceMap.empty)
    controller.get(path)
    path = Path.parseM("http://latin.omdoc.org/math?RingUnit",NamespaceMap.empty)
    controller.get(path)
    path = Path.parseM("http://mathhub.info/MitM/smglom/elliptic_curves?isogeny_class",NamespaceMap.empty)
    controller.get(path)
    path = Path.parseM("http://latin.omdoc.org/type_theories?LambdaPOmega_",NamespaceMap.empty)
    controller.get(path)

    path = Path.parseM("http://cds.omdoc.org/examples?MetaLevelInstances",NamespaceMap.empty)
    controller.get(path)
    println(got.findReplacements())
    */

    val list = Path.parseM("http://mydomain.org/testarchive/mmt-example?test_other",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_all",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_future",NamespaceMap.empty) :: Nil
    val starttime = System.currentTimeMillis()
    //println(got.toXML(got.findReplacements()))
    //println(got.toXML(got.findReplacements(list, true)))
    //println((System.currentTimeMillis()-starttime)/1000)
    controller.handleLine("build testarchive got")
  }
}
