// TO*not*DO: in BackgroundEliminator: also check whether the filename or a directory name between the location and the file is now excluded
// correction: don't allow patterns to be removed. Don't even allow them to be added later via the web interface (security problem).

// TODO: check if port is in use BEFORE starting the server

package info.kwarc.mmt.lf

import zgs.httpd._
import zgs.httpd.let._
import java.io._
import scala.xml._
import scala.collection.mutable._
import java.net._
import java.util.jar.JarInputStream


/** For synchronization of update operations */
object ConflictGuard 


/** Web server administration.
  * @param port the port on which the server runs */
class Server(catalog : Catalog, port : Int) extends HServer {
  
  // Read the XML for the admin page from admin.html. Exit if not found.
  var adminFile = new File("resources" + File.separator + "admin.html")
  if (adminFile == null || !adminFile.canRead) {
    println(Catalog.getOriginalPath(adminFile) + ": critical error: admin file does not exist or cannot be read")
    exit(1)
  }
  
  // The Html data from the admin file
  var adminHtml : scala.xml.Elem = null
  try {
    adminHtml = scala.xml.parsing.XhtmlParser(scala.io.Source.fromFile(adminFile, "utf-8")).head.asInstanceOf[scala.xml.Elem]
  } catch {
    case _ => { 
      println(Catalog.getOriginalPath(adminFile) + ": critical error: admin file cannot be read or contains malformed XML")
      exit(1)
    }
  }
  
  // Read the readme.txt file. Exit if not found.
  var readmeFile = new File("resources" + File.separator + "readme.txt")
  if (readmeFile == null || !readmeFile.canRead) {
    println(Catalog.getOriginalPath(readmeFile) + ": critical error: readme.txt does not exist or cannot be read")
    exit(1)
  }
  
  // Get the text data from readme.txt
  var readmeText : String = null
  try {
    val source = scala.io.Source.fromFile(readmeFile, "utf-8")
    readmeText = source.getLines.toArray.mkString("\n")
    source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it                           
  } catch {
    case e => throw FileOpenError("error: readme.txt cannot be opened or the encoding is not UTF-8")
  }
  
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
  
  override def name = "lfserver"
  override def writeBufSize = 16*1024
  override def tcpNoDelay = true      // make this false if you have extremely frequent requests
  protected def ports = List(port)    // port to listen to
  protected def apps  = List(new RequestHandler) // the request handler
  protected def talkPoolSize = 4
  protected def talkQueueSize = Int.MaxValue
  protected def selectorPoolSize = 2
  
  /** Request handler */
  protected class RequestHandler extends HApp {
    //override def buffered = true
    def resolve(req : HReqHeaderData) : Option[HLet] = {
      if (req.uriPath != "favicon.ico")
        println("Query: " + req.uriPath + "?" + req.query)
        
      req.uriPath match {
      case "favicon.ico" => Some(TextResponse("", None))  // ignore the browser's request for favicon.ico
      case "help" | "" => {
        Some(TextResponse(readmeText, None))
      }
      case "admin" => {
        if (req.query == "") {
          Some(HTMLResponse(updateLocations(adminHtml).toString))
        }
        else if (req.query.startsWith("addLocation=")) { ConflictGuard.synchronized { 
          try {
            catalog.addStringLocation(java.net.URLDecoder.decode(req.query.substring("addLocation=".length), "UTF-8"))
          } catch {
            case InexistentLocation(msg) => println(msg)
          }
          Some(HTMLResponse(updateLocations(adminHtml).toString))
        }}
        else if (req.query.startsWith("deleteLocation=")) { ConflictGuard.synchronized { 
          try {
            catalog.deleteStringLocation(java.net.URLDecoder.decode(req.query.substring("deleteLocation=".length), "UTF-8"))
          } catch {
            case InexistentLocation(msg) => println(msg)
          }
          Some(HTMLResponse(updateLocations(adminHtml).toString))
        }}
        else if (req.query.startsWith("addInclusion=")) { ConflictGuard.synchronized { 
          catalog.addInclusion(java.net.URLDecoder.decode(req.query.substring("addInclusion=".length), "UTF-8"))
          Some(HTMLResponse(updateLocations(adminHtml).toString))
        }}
        else if (req.query.startsWith("addExclusion=")) { ConflictGuard.synchronized { 
          catalog.addExclusion(java.net.URLDecoder.decode(req.query.substring("addExclusion=".length), "UTF-8"))
          Some(HTMLResponse(updateLocations(adminHtml).toString))
        }}
        else Some(TextResponse("Invalid query: " + req.query, None))
      }
      case "crawlAll"  => { ConflictGuard.synchronized { 
        catalog.crawlAll
        Some(HTMLResponse(updateLocations(adminHtml).toString))
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
            if (position.isDefined)
                Some(TextResponse(text, Some(Pair("X-Source-url", "file:" + position.get))))
            else
                Some(TextResponse(text, None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI or URL: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI or URL: " + stringUri, None))
            case FileOpenError(s)   => Some(TextResponse(s, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
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
            Some(TextResponse(position, None))
          } catch {
            case e: java.net.URISyntaxException => Some(TextResponse("Invalid URI: " + stringUri, None))
            case CatalogError(s)    => Some(TextResponse("Unknown URI: " + stringUri, None))
          }
        }
        else Some(TextResponse("Invalid query: " + req.query + "\nuri=URI expected", None))
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
    }}
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
          println(headerTag + ": " + headerContent)
          tk.setHeader(headerTag, headerContent)
      }
      println(text)
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

/** Run the web server */
object Run {
  val usage = """Usage: java -jar lfcatalog.jar (--port <port>)? (location|+inclusion|-exclusion)*
  
  location === absolute path to a file or directory
  inclusion === name
  exclusion === name
  name === file or directory name pattern, without its path. Star is the only special character and matches any sequence of characters.
  
  A folder is crawled iff it doesn't match any exclusion pattern.
  A file is crawled iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains.
  The default port is 8080 on localhost."""
  
  /** Port on which the server runs. Default value is 8080 */
  private var port = 8080
  
  /** Interval, in seconds, between two automatic crawls. Default value is 5 sec */
  private var crawlingInterval = 5
  
  /** Interval, in seconds, between two automatic deletions (from hashes) of files that no longer exist on disk. Default value is 17 sec */
  private var deletingInterval = 17
  
  /** Server thread */
  private var server : Server = null
  
  /** Catalog (catalog) */
  private var catalog : Catalog = null
  
  /** A thread that reads commands from standard input */
  class ConsoleInput(val catalog: Catalog, val server: Server) extends Thread {
      override def run {
          while (true) {
              val input = scala.Console.readLine.trim
              val words : Array[String] = input.split("\\s")
              if (words.length >= 1)
                if (words(0) == "exit") {          // exit
                    server.stop   // stop the server
                    exit(0)       // exit the program
                }
                else if (words(0) == "delete") {    // delete a location
                    if (words.length >= 2)
                        try {
                            catalog.deleteStringLocation(words(1))
                        } catch {
                            case InexistentLocation(msg) => println(msg)
                        }
                    else
                        println("error: delete must be followed by a location address")
                }
                else {          // add a location
                    try {
                        catalog.addStringLocation(words(0))
                    } catch {
                        case InexistentLocation(msg) => println(msg)
                    }
                }
          }
      }
  }
  
  /** A thread that checks for updated files and crawls them every crawlingInterval seconds */
  class BackgroundCrawler(val catalog: Catalog, val crawlingInterval: Int) extends Thread {
    override def run {
      while(true) {
        Thread.sleep(crawlingInterval * 1000)
        catalog.crawlAll
      }
    }
  }
  
  /** A thread that checks for deleted files every deletingInterval seconds and eliminates them from the hashes */
  class BackgroundEliminator(val catalog: Catalog, val deletingInterval: Int) extends Thread {
    override def run {
      while(true) {
        Thread.sleep(deletingInterval * 1000)
        for ((url,_) <- catalog.urlToDocument) {
          val file = new File(URLDecoder.decode(url.toString, "UTF-8"))  // get the file handle from its disk address
          if (!file.exists) {
            println(Catalog.getOriginalPath(file) + ": cannot find file. Uncrawling...")
            catalog.uncrawl(url.toString)
          }
        }
      }
    }
  }
  
  
  def main(args : Array[String]) {
    catalog = new Catalog
    if (!args.isEmpty) {
      var patternsAndLocations : Array[String] = args
      // read the optional --port argument
      if (args.head == "--port")
        if (args.length < 2) { 
          println("error: port number expected\n\n" + usage); exit(1)
        }
        else {
          try { port = Integer.parseInt(args(1)) }
          catch { case _ => println("error: port number expected\n\n" + usage); exit(1) }
          patternsAndLocations = patternsAndLocations.drop(2)
        } 
      // read the patterns and locations
      for (s <- patternsAndLocations)
        if (s.startsWith("-"))   // an exclusion pattern
          catalog.addExclusion(s.drop(1))
        else if (s.startsWith("+")) // an inclusion pattern
          catalog.addInclusion(s.drop(1))
        else {   // a location
          try {
            catalog.addStringLocation(s)
          } catch {
            case InexistentLocation(msg) => println(msg)
          }
        }
    }
    try {
        server = new Server(catalog, port)
        server.start
    } catch {
        case e : java.net.BindException => {
            println("error: port " + port + " is already in use. Choose a different port with the --port argument")
            exit(1)
        }
    }
    new ConsoleInput(catalog, server).start
    new BackgroundCrawler(catalog, crawlingInterval).start
    new BackgroundEliminator(catalog, deletingInterval).start
    println("go to: http://127.0.0.1:8080")
  }
}