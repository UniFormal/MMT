package info.kwarc.mmt.api.lf
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.backend._

import java.io.File
import scala.collections.mutable.HashSet


class Twelf(path : java.io.File) extends Compiler {
   val kind = "twelf"
   val catalog = new Catalog(new HashSet[String]()+path.getPath, new HashSet[String]+"*.elf", new HashSet[String]+".svn", 8081)
   val catalogURI = "http://localhost:" + catalog.port + "/getText"
   
   /** Initialize the catalog
     * @throws PortUnavailable if the port is in use */
   def init {
      catalog.init
   }
   def destroy {
      catalog.destroy
   }
   def check(in: File, targetdir: File) {
      val proc = new java.lang.ProcessBuilder(path.toString, "-omdoc ", targetdir.toString, "-catalog", catalogURI, in.toString)
      proc.start()
   }
}