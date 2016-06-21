package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

trait ScalaCode {self: Archive =>

  private var loader: java.net.URLClassLoader = null

  def loadJava(controller: Controller, cls: String): Unit = {
    if (loader == null) {
      val jar = File(root / (id + ".jar"))
      try {
        loader = new java.net.URLClassLoader(Array(jar.toURI.toURL))
      } catch {
        case _: Exception =>
          log("could not create class loader for " + jar.toString)
          return
      }
    }
    // scala appends $ to classes generated for objects
    // and uses "MODULE$" as the name of the companion object
    val clsJ = loader.loadClass(cls + ".NAMESPACE$")
    //val clsJ = java.lang.Class.forName(cls + ".NAMESPACE$")
    val ns = clsJ.getField("MODULE$").get(clsJ).asInstanceOf[uom.DocumentScala]
    // run tests
    ns.test(controller, (s: String) => log(s))
  }

  /** Integrate scala into a dimension */
  def integrateScala(controller: Controller, in: FilePath = EmptyPath): Unit = {
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(inFile, inPath) =>
      val mpath = Archive.ContentPathToMMTPath(inPath)
      val mod = controller.globalLookup.getModule(mpath)
      val scalaFile = (root / "scala" / inPath).setExtension("scala")
      uom.Integrator.doModule(controller, mod, scalaFile)
      val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">
        {mod.toNode}
      </omdoc>
      xml.writeFile(omdocNode, inFile)
    }
  }
}
