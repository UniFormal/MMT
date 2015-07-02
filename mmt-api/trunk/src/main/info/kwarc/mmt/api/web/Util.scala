package info.kwarc.mmt.api.web

import scala.xml._

 
import scala.util.parsing.json.{JSONType, JSONArray, JSONObject}

import info.kwarc.mmt.api._
import libraries._
import ontology._
import modules._

/**
 * utility functions for web services
 */
object Util {   
  /** returns a stream to a resource from within the JAR file or (if run from classes) the file system
    * @param path to the resource, starting with a slash, e.g. /some-path/resource.txt. 
    * If the resource is in the JAR, then the JAR must look like 
    *       jar-name.jar/some-path/resource.txt
    * If the resource is a separate file and the code is run from classes, the folder structure must look like
    *       project-folder
    *        |--XXX/YYY/info/...
    *        |--resources/mmt-web/ 
    * @return stream to the resource, if found, null otherwise. **The caller must close the stream after reading!**
    */
  def loadResource(path : String) : java.io.InputStream = {
    val stream = getClass.getResourceAsStream("/mmt-web/" + path)  // the file inside the JAR
    if (stream != null) {
        stream  
    } else {
        // run from classes
        val file = utils.MMTSystem.rootFolder/"resources"/"mmt-web"/path
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