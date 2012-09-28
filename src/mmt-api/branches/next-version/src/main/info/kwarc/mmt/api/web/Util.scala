package info.kwarc.mmt.api.web

import scala.xml._
import scala.util.parsing.json.{JSONType, JSONArray, JSONObject}

import info.kwarc.mmt.api._
import libraries._
import ontology._
import modules._

object Util {
   def div(n: List[Node]) : Node = xml.Elem(null, "div", Null, NamespaceBinding(null, utils.xml.namespace("xhtml"), TopScope), n :_*)
   def div(s: String) : Node = div(List(scala.xml.Text(s)))
   /*
   def link(xhtml : NodeSeq, p : Path) = {
     val text = BindHelpers.bind("i", xhtml, "last" -> p.last, "full" -> p.toPath)
     <span jobad:href={p.toPath}>{text}</span>
   }*/
   def ahref(p: Path) =
      <a href="#" onclick={navigate(p)}>{p.last}</a>
   def navigate(p: Path) = 
      "latin_navigate('" + p.toPath + "')"
   
   /** yields a breadcrumb navigation bar as a sequence of links */
   def breadcrumbs(path : Path) : Node = {
      val ancs = path.ancestors.reverse
      val gsep = <span>?</span>
      val lsep = <span>/</span>
      var mpathfound = false
      var spathfound = false
      val crumbs = ancs.flatMap {p =>
         val sep = p match {
         case p : MPath if ! mpathfound => mpathfound = true; gsep
         case p : GlobalName if ! spathfound => spathfound = true; gsep
         case p if p.^! == p => Nil
         case _ => lsep
       }
       sep ++ <span jobad:href={p.toPath} class="crumb">{p.last}</span>
      }
      // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
      //<div xmlns={utils.xml.namespace("xhtml")} xmlns:jobad={utils.xml.namespace("jobad")}>{crumbs}</div>
      xml.Elem(null, "div", Null, NamespaceBinding(null, utils.xml.namespace("xhtml"),
                              NamespaceBinding("jobad", utils.xml.namespace("jobad"), TopScope)), crumbs : _*)
   }
   
   def item(p : Path, state : String, label : Option[String] = None) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={navigate(p)}>{label.getOrElse(p.last)}</name></content>
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
    *       compiled-folder
    *        |--top-level-package-name/../class-files
    *        |--some-path/resource.txt 
    * @return stream to the resource, if found, null otherwise. **The caller must close the stream after reading!**
    */
  def loadResource(path : String) : java.io.InputStream = {
    val stream = getClass.getResourceAsStream("/mmt-web/" + path)  // the file inside the JAR
    if (stream != null) //false
        return stream
    else {
        val filePath : String = try {
          val binaryFolder : java.io.File = new java.io.File(getClass.getProtectionDomain.getCodeSource.getLocation.toString)  // e.g. .../lfcatalog/trunk/build/main
          val resourceFolder : String = binaryFolder.getParentFile.getParentFile.toString + "/resources/mmt-web"  // e.g. .../lfcatalog/trunk/resources/mmt-web
          (if (resourceFolder.startsWith("file:")) resourceFolder.substring("file:".length) else resourceFolder) + "/" + path
        }
        catch {
          case _ => return null
        }
        //println("trying to get resource from "  + filePath)
        val file = new java.io.File(filePath)  // the file on disk
        // Try reading from compiled-folder/some-path/resource.txt
        if (file.exists)
          return new java.io.FileInputStream(file)
        else
          return null
    }
  }
  
  /** "/a/bb/" gets split to ("", "a", "bb") */
  def getComponents(path : String) : List[String] = path.split("/").toList
  
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
        return portTaken
  }
}