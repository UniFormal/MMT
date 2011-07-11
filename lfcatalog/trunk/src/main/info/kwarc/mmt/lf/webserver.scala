package info.kwarc.mmt.lf

import zgs.httpd._
import zgs.httpd.let._

import java.io._
import java.net._

import scala.xml._
import scala.collection.mutable._


/** An HTTP RESTful server.
  * @param catalog the main controller and storage
  * @param port the port on which the server runs */
class WebServer(catalog : Catalog, port : Int) extends HServer {
  
  /** Administration page, read from jar://resources/admin.html */
  var adminHtml : Option[scala.xml.Elem] = None
  
  /** Error page returned by the server */
  val adminError = "<html><body>Bummer, can't find jar://resources/admin.html.</body></html>"
  
  /** Readme page, read from jar://resources/readme.txt */
  var readmeText : Option[String] = None
  
  /** Error page returned by the server */
  val readmeError = "Bummer, can't find jar://resources/readme.html."
    
  
  // Read jar://resources/admin.html
  Option(getClass.getResourceAsStream("/resources/admin.html")) match {
    case None => 
      catalog.log("warning: /resources/admin.html inside JAR does not exist")   // ignore if not found
    case Some(adminFile) =>
      var source : scala.io.BufferedSource = null
      try {
        source = scala.io.Source.fromInputStream(adminFile, "UTF-8")
        adminHtml = Option(scala.xml.parsing.XhtmlParser(source).head.asInstanceOf[scala.xml.Elem])
      } catch {
        case _ =>
          catalog.log("critical error: /resources/admin.html inside JAR contains malformed XML")   // this should never happen
          exit(1)
      } finally {
          source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
      }
  }
  
  // Read jar://resources/readme.txt
  Option(getClass.getResourceAsStream("/resources/readme.txt")) match {
    case None => 
      catalog.log("warning: /resources/readme.txt inside JAR does not exist or cannot be read")   // ignore if not found
    case Some(readmeFile) =>
      var source : scala.io.BufferedSource = null
      try {
        source = scala.io.Source.fromInputStream(readmeFile, "UTF-8")
        readmeText = Option(source.getLines.toArray.mkString("\n"))                           
      } catch {
          case e =>
            catalog.log("critical error: /resources/readme.txt inside JAR cannot be opened in the UTF-8 encoding")    // this should never happen
            exit(1)
      } finally {
          source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
      }
  }
 
  
  override def name = "lfserver"
  override def writeBufSize = 16*1024
  override def tcpNoDelay = true      // make this false if you have extremely frequent requests
  protected def ports = List(port)    // port to listen to
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
     case Elem(a, b, c, d, ch @_*) => Elem(a, b, c, d, ch.map(updateLocations) : _*)
     case other => other
  }
  
  
  /** Request handler */
  protected class RequestHandler extends HApp {
    //override def buffered = true
    def resolve(req : HReqHeaderData) : Option[HLet] = {
      if (req.uriPath != "favicon.ico")
        print(Time + "Query: " + req.uriPath + "?" + req.query + " ")
        
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
            case InexistentLocation(msg) => catalog.log(Time + msg)
          }
          Some(HTMLResponse(adminHtml.map(updateLocations).getOrElse(adminError).toString))
        }}
        else if (req.query.startsWith("deleteLocation=")) { ConflictGuard.synchronized { 
          try {
            catalog.deleteStringLocation(java.net.URLDecoder.decode(req.query.substring("deleteLocation=".length), "UTF-8"))
          } catch {
            case InexistentLocation(msg) => catalog.log(Time + msg)
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
                         exit(0)      // exit the program
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
                print("OK")
                Some(TextResponse(text, Some(Pair("X-Source-url", "file:" + position.get))))
            }
            else {
                print("NOT FOUND")
                Some(TextResponse(text, None))
            }
          } catch {
              case e: java.net.URISyntaxException => {print("NOT FOUND"); Some(TextResponse("Invalid URI or URL: " + stringUri, None))}
              case CatalogError(s)    => {print("NOT FOUND"); Some(TextResponse("Unknown URI or URL: " + stringUri, None))}
              case FileOpenError(s)   => {print("NOT FOUND"); Some(TextResponse(s, None))}
          }
        }
        else {print("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
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
            print("OK")
            Some(TextResponse(position, Some(Pair("X-Source-url", "file:" + position))))
          } catch {
              case e: java.net.URISyntaxException => {print("NOT FOUND"); Some(TextResponse("Invalid URI: " + stringUri, None))}
              case CatalogError(s)    => {print("NOT FOUND"); Some(TextResponse("Unknown URI: " + stringUri, None))}
          }
        }
        else {print("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
      }
      case "getPositionInHeader" => {
        if (req.query.startsWith("uri=")) {
          var stringUri : String = null
          try {
            stringUri = java.net.URLDecoder.decode(req.query.substring("uri=".length), "UTF-8")
            val position = catalog.getPosition(stringUri)
            print("OK")
            Some(TextResponse("", Some(Pair("X-Source-url", "file:" + position))))
          } catch {
              case e: java.net.URISyntaxException => {print("NOT FOUND"); Some(TextResponse("Invalid URI: " + stringUri, None))}
              case CatalogError(s)    => {print("NOT FOUND"); Some(TextResponse("Unknown URI: " + stringUri, None))}
          }
        }
        else {print("NOT FOUND"); Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))}
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
      case "getNSIntroduced" => {
        if (req.query.startsWith("url=")) {
          var stringUrl : String = null
          try {
            stringUrl = java.net.URLDecoder.decode(req.query.substring("url=".length), "UTF-8")
            val URIs = catalog.getNSIntroduced(stringUrl)
            Some(TextResponse(URIs.mkString("\n"), None))
          } catch {
            case CatalogError(s)    => Some(TextResponse(s, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nurl=URL expected", None))
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
  private def TextResponse(text : String, header : Option[Pair[String, String]]) : HLet = new HLet {
    def act(tk : HTalk) {
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
        .close
    }
  }
  
  /** An HTML response that the server sends back to the browser
    * @param text the HTML message that is sent in the HTTP body
    */
  private def HTMLResponse(text : String) : HLet = new HLet {
    def act(tk : HTalk) {
      val out = text.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
            .setContentType("text/html; charset=utf8")
            .write(out)
            .close
    }
  }
}