package info.kwarc.mmt.twelf

import tiscaf._
import tiscaf.let._

import java.io._
import java.net._

import scala.xml._

/*
 * TODO known issue: if resources are scarce on windows,
 * the catalog may respond correctly but Twelf only receives a socket error (win32 error code 10055)
 * it's unclear how to increase available buffer space to avoid this problem
 */

/** An HTTP RESTful server.
  * @param catalog the main controller and storage
  * @param port the port on which the server runs */
class WebServer(catalog : Catalog, port : Int) extends HServer {

  // log all the things
  override def error(msg: String, t: Throwable) = catalog.log(msg)
  override def warning(msg: String) = catalog.log(msg)
  override def info(msg: String) = catalog.log(msg)

  /** Administration page, read from jar://resources/admin.html */
  var adminHtml : Option[scala.xml.Elem] = None

  /** Error page returned by the server */
  val adminError = "<html><body>cannot find jar://server-resources/admin.html.</body></html>"

  /** Readme page, read from jar://resources/readme.txt */
  var readmeText : Option[String] = None

  /** Error page returned by the server */
  val readmeError = "cannot find jar://server-resources/readme.html."

  // Read jar://resources/admin.html
  try {
    loadResource("/server-resources/admin.html", "UTF-8") match {
      case Some(bufferedSource) =>
         try {
           adminHtml = Option(scala.xml.parsing.XhtmlParser(bufferedSource).head.asInstanceOf[scala.xml.Elem])
         } catch {
           case _: Exception =>
              catalog.log("critical error: /server-resources/admin.html inside JAR contains malformed XML")   // this should never happen
              sys.exit(1)
         } finally {
              bufferedSource.close       // close the file, since scala.io.Source doesn't close it
         }
      case None => catalog.log("warning: /server-resources/admin.html does not exist")
    }
  }
  catch {
    case EncodingException(msg) => catalog.log(msg); sys.exit(1)
  }

  // Read jar://resources/readme.txt
  try {
    loadResource("/server-resources/readme.txt", "UTF-8") match {
      case Some(bufferedSource) =>
         readmeText = Option(bufferedSource.getLines().toArray.mkString("\n"))
         bufferedSource.close       // close the file, since scala.io.Source doesn't close it
      case None => catalog.log("warning: /server-resources/readme.txt does not exist")
    }
  }
  catch {
    case EncodingException(msg) => catalog.log(msg); sys.exit(1)
  }

  override def name = "lfserver"
  //override def writeBufSize = 16*1024
  override def tcpNoDelay = true      // make this false if you have extremely frequent requests
  override def startStopListener = {}  // prevents tiscaf from creating a "stop" listener
/*
  override def onMessage(s: String) {
     catalog.log(s)
  }
*/
  protected def ports = Set(port)    // port to listen to
  protected def apps  = List(new RequestHandler) // the request handler
  protected def talkPoolSize = 4
  protected def talkQueueSize = Int.MaxValue
  protected def selectorPoolSize = 2


  /** A recursive function that replaces:
    * 1. <locations/> with the bullet list of locations
    * 2. <inclusions/> with the bullet list of inclusion patterns
    * 3. <exclusions/> with the bullet list of exclusion patterns */
  private def updateLocations(node : Node) : Node = node match {
     case <locations/> => {<ul>{ catalog.locations.flatMap(f =>
                                    <li>
                                        <form action="admin" method="get">
                                            <input type="hidden" name="deleteLocation" value={ Catalog.getPath(f) } />
                                            <input type="submit" value="Delete" />
                                        </form>
                                        { Catalog.getOriginalPath(f) }
                                    </li>) }</ul> }
     case <inclusions/> => {<ul>{ catalog.getInclusions.flatMap(s => <li>{ s }</li>) }</ul> }
     case <exclusions/> => {<ul>{ catalog.getExclusions.flatMap(s => <li>{ s }</li>) }</ul> }
     case Elem(a, b, c, d, ch @_*) => Elem(a, b, c, d, true, ch.map(updateLocations) : _*)
     case other => other
  }


  /** Request handler */
  protected class RequestHandler extends HApp {
    //override def buffered = true
    def resolve(req : HReqData) : Option[HLet] = {
      if (req.uriPath != "favicon.ico")
        catalog.log(Time.toString + "Query: " + req.uriPath + "?" + req.query + " ")

      val response : Option[HLet] = req.uriPath match {
      case "favicon.ico" => Some(TextResponse("", None))  // ignore the browser's request for favicon.ico
      case "help" | "" => {
        Some(TextResponse(readmeText.getOrElse(readmeError), None))
      }
      case "admin" => {
        if (req.query == "") {
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }
        else if (req.query.startsWith("addLocation=")) { ConflictGuard.synchronized {
          try {
            catalog.addStringLocation(java.net.URLDecoder.decode(req.query.substring("addLocation=".length), "UTF-8"))
          } catch {
            case InexistentLocation(msg) => catalog.log(Time.toString + msg)
          }
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }}
        else if (req.query.startsWith("deleteLocation=")) { ConflictGuard.synchronized {
          try {
            catalog.deleteStringLocation(java.net.URLDecoder.decode(req.query.substring("deleteLocation=".length), "UTF-8"))
          } catch {
            case InexistentLocation(msg) => catalog.log(Time.toString + msg)
          }
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }}
        else if (req.query.startsWith("addInclusion=")) { ConflictGuard.synchronized {
          catalog.addInclusion(java.net.URLDecoder.decode(req.query.substring("addInclusion=".length), "UTF-8"))
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }}
        else if (req.query.startsWith("addExclusion=")) { ConflictGuard.synchronized {
          catalog.addExclusion(java.net.URLDecoder.decode(req.query.substring("addExclusion=".length), "UTF-8"))
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }}
        else Some(TextResponse("Invalid query: " + req.query, None))
      }
      case "crawlAll"  => { ConflictGuard.synchronized {
        catalog.crawlAll
        Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
      }}
      case "exit"   => {stop         // stop the server
                         sys.exit(0)      // exit the program
                         None}
      case "getMetaText" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val commentText = catalog.getMeta(stringUri, true)
            Some(TextResponse(commentText, None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI or URL: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI or URL: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
      }
      case "getMeta" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val commentText = catalog.getMeta(stringUri)
            Some(TextResponse(commentText, None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI or URL: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI or URL: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
      }
      case "getText" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val text = catalog.getText(stringUri)
            var position : Option[String] = None
            try {
                position = Some(catalog.getPosition(stringUri))
            }
            catch {
                case CatalogError(s) =>  // if the uri does not point to a module, cst or structure declaration, don't return the position
            }
            if (position.isDefined) {
                catalog.log("OK")
                Some(TextResponse(text, Some(("X-Source-url", "file:" + position.get))))
            }
            else {
                catalog.log("NOT FOUND")
                Some(TextResponse(text, None))
            }
          } catch {
              case e: java.net.URISyntaxException => {catalog.log("NOT FOUND"); Some(TextResponse("Invalid URI or URL: " + stringUri, None))}
              case CatalogError(s)    => {catalog.log("NOT FOUND"); Some(TextResponse("Unknown URI or URL: " + stringUri, None))}
              case FileOpenError(s)   => {catalog.log("NOT FOUND"); Some(TextResponse(s, None))}
          }
        }
        else {catalog.log("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
      }
      case "getDependencies" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val dependencies = catalog.getDependencies(stringUri)
            Some(TextResponse(dependencies.mkString("\n"), None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
      }
      case "getChildren" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val children = catalog.getChildren(stringUri)
            Some(TextResponse(children.mkString("\n"), None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
      }
      case "getPosition" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val position = catalog.getPosition(stringUri)
            catalog.log("OK")
            Some(TextResponse(position, Some(("X-Source-url", "file:" + position))))
          } catch {
              case e: java.net.URISyntaxException => {catalog.log("NOT FOUND"); Some(TextResponse("Invalid URI: " + stringUri, None))}
              case CatalogError(s)    => {catalog.log("NOT FOUND"); Some(TextResponse("Unknown URI: " + stringUri, None))}
          }
        }
        else {catalog.log("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
      }
      case "getPositionInHeader" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val position = catalog.getPosition(stringUri)
            catalog.log("OK")
            Some(TextResponse("", Some(("X-Source-url", "file:" + position))))
          } catch {
              case e: java.net.URISyntaxException => {catalog.log("NOT FOUND"); Some(TextResponse("Invalid URI: " + stringUri, None))}
              case CatalogError(s)    => {catalog.log("NOT FOUND"); Some(TextResponse("Unknown URI: " + stringUri, None))}
          }
        }
        else {catalog.log("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
      }
      case "getOmdoc" => {
        if (req.query.startsWith("url=")) {
          var stringUrl : String = null
          try {
            stringUrl = java.net.URLDecoder.decode(req.query.substring("url=".length), "UTF-8")
            val omdoc = catalog.getOmdoc(stringUrl)
            Some(TextResponse(omdoc, None))
          } catch {
            case CatalogError(s)    => Some(TextResponse(s, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nurl=URL expected", None))
      }
      case "getNamespaces" => {
        if (req.query.startsWith("url=")) {
          var stringUrl : String = null
          try {
            stringUrl = java.net.URLDecoder.decode(req.query.substring("url=".length), "UTF-8")
            val URIs = catalog.getNamespaces(stringUrl)
            Some(TextResponse(URIs.mkString("\n"), None))
          } catch {
            case CatalogError(s)    => Some(TextResponse(s, None))
          }
        }
        else if (req.query.isEmpty)
            Some(TextResponse(catalog.getNamespaces().mkString("\n"), None))
        else Some(TextResponse("Invalid query: " + req.query + "\nurl=URL expected", None))
      }
      case "getModules" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val modules = catalog.getModulesInNamespace(stringUri)
            Some(TextResponse(modules.mkString("\n"), None))
          } catch {
              case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI: " + stringUri, None))
              case CatalogError(s)    => Some(TextResponse("Unknown URI: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
      }
      case _        => Some(TextResponse("Invalid path: " + req.uriPath, None))
    }
    catalog.log("")
    response
    }
  }

  /** A text response that the server sends back to the browser
    * @param text the message that is sent in the HTTP body
    * @param header optionally, a custom header tag, given as Pair(tag, content) that is added to the HTTP header
    */
  private def TextResponse(text : String, header : Option[(String, String)]) : HLet = new HSimpleLet {
    def act(tk : HTalk): Unit = {
      val out = text.getBytes("UTF-8")
      // !sending data in the header as well!
      // prepare data for sending within a header: replace \r with `r, \n with `n, ` with `o
      /*val escapedText : String = text.replaceAll("`","`o")
                                     .replaceAll("\r","`r")
                                     .replaceAll("\n","`n")*/
      tk.setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
      if (header.isDefined) {
          val headerTag = header.get._1
          val headerContent = header.get._2
          //catalog.log(Time + headerTag + ": " + headerContent)
          tk.setHeader(headerTag, headerContent)
      }
      //catalog.log(Time + text)
      tk.write(out)
    }
  }

  /** An HTML response that the server sends back to the browser
    * @param text the HTML message that is sent in the HTTP body
    */
  private def HTMLResponse(text : String) : HLet = new HSimpleLet {
    def act(tk : HTalk): Unit = {
      val out = text.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
            .setContentType("text/html; charset=utf8")
            .write(out)
    }
  }
}
