package info.kwarc.mmt.api.lf
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.backend._

import java.io.File
import scala.collection.mutable.HashSet

/** Utility for starting the catalog and calling the Twelf compiler
  * @param path the Twelf compiler executable
  */
class Twelf(path : java.io.File) extends Compiler {
   val kind = "twelf"
   
   /** Handle for the catalog */
   val catalog = new Catalog(new HashSet[String]()+path.getPath, new HashSet[String]+"*.elf", new HashSet[String]+".svn", 8081)
   
   /** The URI with which to query the catalog's web server */
   val catalogURI = "http://localhost:" + catalog.port + "/getText"
   
   /** Initialize the catalog
     * @throws PortUnavailable if the port is in use */
   def init {
      catalog.init    //  throws PortUnavailable
   }
   def destroy {
      catalog.destroy
   }
   
   /** Compile a Twelf file to OMDoc
     * @param in the input Twelf file 
     * @param targetdir the directory in which to put a generated OMDoc file with the same name as the input file
     */
   def check(in: File, targetdir: File) {
      val proc = new java.lang.ProcessBuilder(path.toString, "-omdoc ", targetdir.toString, "-catalog", catalogURI, in.toString)
      proc.start()
   }
}