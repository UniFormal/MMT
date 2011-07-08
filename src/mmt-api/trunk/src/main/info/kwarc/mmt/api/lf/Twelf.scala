package info.kwarc.mmt.api.lf
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.backend._

import java.io.File

class Catalog(val port: Int)

class Twelf(path : java.io.File) extends Compiler {
   val kind = "twelf"
   val catalog = new Catalog(8081)
   val catalogURI = "http://localhost:" + catalog.port + "/getText"
   def init {
      // start catalog
   }
   def destroy {
      // end catalog
   }
   def check(in: File, targetdir: File) {
      val proc = new java.lang.ProcessBuilder(path.toString, "-omdoc ", targetdir.toString, "-catalog", catalogURI, in.toString)
      proc.start()
   }
}