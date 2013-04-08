package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import utils._
import utils.FileConversion._

trait ScalaArchive extends WritableArchive {
  
    private var loader: java.net.URLClassLoader = null
    def loadJava(controller: Controller, cls: String, addRules: Boolean, runTests: Boolean) {
       if (loader == null) {
          val jar = File(root / (id+".jar"))
          try {
            println(jar.toURI.toURL)
            loader = new java.net.URLClassLoader(Array(jar.toURI.toURL))
          } catch {
            case _:Exception => 
             log("could not create class loader for " + jar.toString)
             return
          }
       }
       // scala appends $ to classes generated for objects
       // and uses "MODULE$" as the name of the companion object
       val clsJ = loader.loadClass(cls + ".NAMESPACE$")
       //val clsJ = java.lang.Class.forName(cls + ".NAMESPACE$")
       val ns = clsJ.getField("MODULE$").get(clsJ).asInstanceOf[uom.DocumentScala]
       if (addRules) {
          ns.register(controller.extman.ruleStore)
       }
       if (runTests) {
          ns.test(controller, (s: String) => log(s))
       }
    }
  
    /** Extract scala from a dimension */
    def extractScala(controller: Controller, in : List[String] = Nil) {
       traverse[String]("content", in, Archive.extensionIs("omdoc")) ({
           //files
           case Current(inFile,inPath) => 
                 val mpath = Archive.ContentPathToMMTPath(inPath)
                 val mod = controller.globalLookup.getModule(mpath)
                 val outFile = (root / "scala" / inPath).setExtension("scala")
                 outFile.getParentFile.mkdirs
                 uom.Extractor.doModule(controller, mod, outFile)
       }, {
           //directories
          (curr: Current, results: List[String]) =>
             if (! curr.path.isEmpty) {
                val dpath = Archive.ContentPathToDPath(curr.path)
                val outFile = (root / "scala" / curr.path / "NAMESPACE.scala")
                uom.Extractor.doFolder(dpath, results, outFile)
             } else ""
       })
    }
    
    /** Integrate scala into a dimension */
    def integrateScala(controller: Controller, in : List[String] = Nil) {
       traverse("content", in, Archive.extensionIs("omdoc")) {case Current(inFile, inPath) =>
              val mpath = Archive.ContentPathToMMTPath(inPath)
              val mod = controller.globalLookup.getModule(mpath)
              val scalaFile = (root / "scala" / inPath).setExtension("scala")
              uom.Integrator.doModule(controller, mod, scalaFile)
              val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{mod.toNode}</omdoc>
              xml.writeFile(omdocNode, inFile)
        }
    }
}