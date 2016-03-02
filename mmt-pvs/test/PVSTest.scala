package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, NamespaceMap, Path}
import info.kwarc.mmt.api.frontend.Controller

/**
 * Created by raupi on 22.07.15.
 */
object PVSTest {
  def main(args: Array[String]) {
    val path = "/home/raupi/lmh/MathHub/PVS/Prelude"
    val controller = new Controller
    controller.handleLine("archive add ../MathHub")
    controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine("extension info.kwarc.mmt.api.archives.BuildQueue")
    controller.handleLine("log console")
    // controller.handleLine("log+ archive")
    // controller.handleLine("log+ debug")
    // controller.handleLine("log+ pvs-omdoc")
    controller.handleLine("mathpath archive ../../MathHub")
    controller.handleLine("build PVS/Prelude mmt-omdoc")
    controller.handleLine("build PVS/Prelude pvs-omdoc")
    // controller.handleLine("build PVS/NASA pvs-omdoc")

    /*
    val p1 = DPath(URI.http colon "pvs.csl.sri.com") ? "PVS"
    val p2 = Path.parseM("http://pvs.csl.sri.com/?PVS",NamespaceMap.empty)
    println(p1==p2)
    println(p1.getClass + "  " + p2.getClass)
    println("^: " + p1.^ + " " + p2.^ + " " + (p1.^ == p2.^))
    println("^!: " + p1.^! + " " + p2.^! + " " + (p1.^! == p2.^!))
    println("^^: " + p1.^^ + " " + p2.^^ + " " + (p1.^^ == p2.^^))
    println("last: " + p1.last + " " + p2.last + " " + (p1.last == p2.last))
    println("module: " + p1.module + " " + p2.module + " " + (p1.module == p2.module))
    println("parent: " + p1.parent + " " + p2.parent + " " + (p1.parent == p2.parent))
    println("toMPath: " + p1.toMPath + " " + p2.toMPath + " " + (p1.toMPath == p2.toMPath))
    val uri1 = p1.^!.doc.uri
    val uri2 = p2.^!.doc.uri
    println("uri: " + uri1 + " " + uri2 + " " + (uri1 == uri2))
    println("absolute: " + uri1.absolute + " " + uri2.absolute)
    println("scheme: " + uri1.scheme + " " + uri2.scheme + " " + (uri1.scheme == uri2.scheme))
    println("authority: " + uri1.authority + " " + uri2.authority + " " + (uri1.authority == uri2.authority))
    println("path: " + uri1.path + " " + uri2.path + " " + (uri1.path == uri2.path))
    println("query: " + uri1.query + " " + uri2.query + " " + (uri1.query == uri2.query))
    println("fragment: " + uri1.fragment + " " + uri2.fragment + " " + (uri1.fragment == uri2.fragment))

   println(controller.globalLookup(p2))
   println(controller.globalLookup(p1))
    */

    // build PVS/NASA pvs-omdoc

    //controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    //controller.handleLine("build PVS/Prelude mmt-omdoc")
    //controller.handleLine("build PVS/Prelude relational")
    //val t1 = controller.get(Path.parse("http://pvs.csl.sri.com/Prelude?bit"))
    //val t2 = controller.get(Path.parse("http://pvs.csl.sri.com/?PVS/defdecl"))
    //implicit val rh = new presentation.StringBuilder
    //val presenter = new MMTSyntaxPresenter
    //controller.extman.addExtension(presenter)
    //presenter(t2)
    //presenter(t1)
    //println(rh.get)
    //println(th.parameters)
    //val th = controller.get(Path.parse("http://pvs.csl.sri.com?PVS")).asInstanceOf[DeclaredTheory]
    //println(th)
    //println(th.getInnerContext)
    //println(th.parameters)

   // val c = controller.get(Path.parse("http://cds.omdoc.org/testcases?DLO?eq_leq")).asInstanceOf[FinalConstant]
   // println(c.tp.get.asInstanceOf[OMBINDC].context)

  }
}