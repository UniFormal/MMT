package info.kwarc.mmt.api.web

import scala.xml._
import scala.util.parsing.json.{JSONType, JSONArray, JSONObject}

import info.kwarc.mmt.api._
import libraries._
import ontology._
import modules._

object Util {
   def item(p : Path, state : String, label : Option[String] = None) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={"mmt.sideBarClick(event, '" + p.toPath + "')"}>{label.getOrElse(p.last)}</name></content>
      </item>
   
   /** returns a string identifying the kind of an edge */
   def edgeKind(e: Edge) : String = e match {
      case ViewEdge(_) => "View"
      case StructureEdge(_) => "Structure"
      case IncludeEdge => "Include"
      case MetaEdge => "Meta"
   }
   /** returns the URI of an edge, empty if none */
   def edgeUri(e: Edge) : String = e match {
      case ViewEdge(p) => p.toPath
      case StructureEdge(p) => p.toPath
      case IncludeEdge => ""
      case MetaEdge => ""
   }
   /** returns short name of an edge, empty if none */
   def edgeName(e: Edge) : String = e match {
      case ViewEdge(p) => p.last
      case StructureEdge(p) => p.last
      case IncludeEdge => ""
      case MetaEdge => ""
   }
   
   
  /** returns a stream to a resource within the JAR file. If it's not found in the JAR, then it looks into the file system
    * @param path to the resource, starting with a slash, e.g. /some-path/resource.txt. 
    * If the resource is in the JAR, then the JAR must look like 
    *       jar-name.jar/some-path/resource.txt
    * If the resource is a separate file and the code is run from outside any JAR, then the folder structure must look like
    *       project-folder
    *        |--bin/main/info/...
    *        |--resources/mmt-web/ 
    * @return stream to the resource, if found, null otherwise. **The caller must close the stream after reading!**
    */
  def loadResource(path : String) : java.io.InputStream = {
    val stream = getClass.getResourceAsStream("/mmt-web/" + path)  // the file inside the JAR
    if (stream != null)
        stream
    else {
        val filePath : String = try {
          //the folder containing the class files
          val binaryFolder : java.io.File = new java.io.File(getClass.getProtectionDomain.getCodeSource.getLocation.toString)  // e.g. .../lfcatalog/trunk/build/main
          val resourceFolder : String = binaryFolder.getParentFile.toString + "/resources/mmt-web"
          (if (resourceFolder.startsWith("file:")) resourceFolder.substring("file:".length) else resourceFolder) + "/" + path
        }
        catch {
          case _ : Throwable => return null
        }
        //println("trying to get resource from "  + filePath)
        val file = new java.io.File(filePath)  // the file on disk
        // Try reading from compiled-folder/some-path/resource.txt
        if (file.exists)
           new java.io.FileInputStream(file)
        else
           null
    }
  }
  
  /** Checks whether a port is used
    * @param port port number to be checked
    * @return true if the port is used, false if it is available
    */
  def isTaken(port: Int) : Boolean = {
        var portTaken = false
        var socket : java.net.ServerSocket = null
        try {
            socket = new java.net.ServerSocket(port)
        } catch {
            case e : java.io.IOException => portTaken = true
        } finally {
            // Clean up
            if (socket != null) {
                socket.setReuseAddress(true)    // sets an OS flag that the port is available again
                socket.close
            }
        }
        portTaken
  }
}