package info.kwarc.mmt.twelf

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.net.URLDecoder
import java.util.regex.Pattern
import scala.xml.PrettyPrinter

//import scala.xml._
import scala.collection.mutable.{HashSet, LinkedHashSet, LinkedHashMap, HashMap, ListBuffer}
import scala.collection.immutable.SortedSet


/** Utility object */
object Catalog {
    /** Get the physical path to a file or folder, NOT %-encoded
      * @param location the file descriptor
      * @return the path as a string */
    def getOriginalPath(location: File) : String = location.toURI.normalize.getPath

    /** Get the physical path to a file or folder, %-encoded
      * @param location the file descriptor
      * @return the path as a string */
    def getPath(location: File) : String = location.toURI.normalize.getRawPath
}


/** The information maintained by crawling all the locations
  * @param locationsParam set of disk locations to scan, given as strings
  * @param inclusionsParam set of inclusion patterns, given as strings. Default value is *.elf
  * @param exclusionsParam set of exclusion patterns, given as strings. Default value is .svn
  * @param port port on which the server runs. Default value is 8080
  * @param searchPort specifies whether the catalog should search for available ports incrementally if the specified port is not available
  * @param log : String => Unit the function used for logging. Default is a function that logs to stdout
  * @param crawlingInterval interval, in seconds, between two automatic crawls. Default value is 30 sec
  * @param deletingInterval interval, in seconds, between two automatic deletions (from hashes) of files that no longer exist on disk. Default value is 17 sec
  */
class Catalog(val locationsParam: HashSet[String] = HashSet(),
               val inclusionsParam: HashSet[String] = HashSet("*.elf"),
               val exclusionsParam: HashSet[String] = HashSet(".svn"),
               var port: Int = 8080,
               val searchPort: Boolean = false,
               val log: String => Unit = println,
               val crawlingInterval: Int = 30,
               val deletingInterval: Int = 17) {
  import Catalog._

  // ------------------------------- public members -------------------------------


  /** Locations (files and folders) being watched */
  val locations = HashSet[File] ()

  /** Map from URLs to documents. Its key set is the set of file URLs that are currently indexed */
  val urlToDocument = HashMap[URI, Document] ()

  /** Map from URIs to named blocks (modules, declarations or assignments) */
  val uriToNamedBlock = HashMap[URI, NamedBlock] ()

  /** Map from namespace URIs to modules declared in that URI */
  val uriToModulesDeclared = HashMap[URI, LinkedHashSet[URI]] ()


  // ------------------------------- private members -------------------------------


  /** Exclusion patterns for files and folders. */
  private val exclusions = HashSet[String] ()

  /** Inclusion patterns for files */
  private val inclusions = HashSet[String] ()

  /** Processed exclusion and inclusion patterns (for internal use)
    * Everything is quoted, except *, which are replaced with .*       */
  private val processedExclusions = HashSet[String] ()
  private val processedInclusions = HashSet[String] ()

  /** the RESTful web server */
  private var server : WebServer = null

  /** background processes */
  private val bkgCrawler = new BackgroundCrawler(this, crawlingInterval)
  private val bkgEliminator = new BackgroundEliminator(this, deletingInterval)


  // ------------------------------- initialization and destruction -------------------------------


  /** Start the web server, background threads, add given patterns & locations
    * @return the port number, which can be different from the port specified in the constructor
    * @throws PortUnavailable if the web server cannot be started because the specified port is already in use */
  def init : Int = {
      // check if port is available
      if (isTaken(port))
        if (!searchPort)
            throw PortUnavailable(port.toString)
        else {  // look for another open port, incrementally
            var newPort = port + 1
            while (isTaken(newPort) && newPort <= port + 100)
                newPort = newPort + 1
            if (newPort == port + 101)
                throw PortUnavailable(port.toString)
            else port = newPort
        }

      // construct server (it reads the resource pages when constructed)
      server = new WebServer(this, port)

      // start background threads that make sure this catalog is synchronized with the disk
      bkgCrawler.start
      bkgEliminator.start

      // add inclusion and exclusion patterns
      inclusionsParam.foreach(addInclusion)
      exclusionsParam.foreach(addExclusion)

      // add locations
      for (l <- locationsParam)
          try {
              addStringLocation(l)
          } catch {
              case InexistentLocation(msg) => log(msg)
          }

      // start the web server (different threads)
      server.start
      log("go to: http://127.0.0.1:" + port)
      return port
  }


  /** Stop the web server */
  def destroy: Unit = {
      server.stop
      bkgCrawler.interrupt()
      bkgEliminator.interrupt()
      locations.clear()
      urlToDocument.clear()
      uriToNamedBlock.clear()
      uriToModulesDeclared.clear()
  }


  // ------------------------------- getter methods -------------------------------


  /** The URI for querying the catalog with getText */
  def queryURI : String = "http://localhost:" + port + "/getPositionInHeader"


  /** Exclusion patterns for files and folders.
    * Star is the only special character and matches any sequence of characters.
    * A folder is crawled iff it doesn't match any exclusion pattern.
    * A file is crawled iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains. */
  def getExclusions : HashSet[String] = exclusions

  /** Inclusion patterns for files
    * Star is the only special character and matches any sequence of characters.
    * A folder is crawled iff it doesn't match any exclusion pattern.
    * A file is crawled iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains. */
  def getInclusions : HashSet[String] = inclusions


  /** Get the semantic comment associated with a document, module, constant or structure
    * @param stringUri URI of a module or URL of a document, given as string
    * @param asText set it to true for text output and to false for XML output
    * @return if asText is true, the semantic comment, copied character by character from the file. Otherwise, an XML representation of the comment
    * @throws java.net.URISyntaxException if stringUri does not point to a location on disk and, as a URI, it is not valid
    * @throws CatalogError(s: String) if the URI/URL is unknown */
  def getMeta(stringUri : String, asText : Boolean = false) : String = {
    val document = new File(stringUri)
    var uri : URI = null
    try {
      // First see whether stringUri points to a file on the disk
      if (document == null)
        throw CatalogError("")
      uri = new URI(getPath(document))
      if (urlToDocument.isDefinedAt(uri))
        if (asText == true)
          urlToDocument(uri).associatedComment.map(_.comment).getOrElse("")
        else
          urlToDocument(uri).associatedComment.map(x => new PrettyPrinter(80,2).format(x.toOmdoc)).getOrElse("")
      else
        throw CatalogError("")
    } catch {   // If it doesn't point to a file on the disk, maybe it's a module URI
      case _ : Throwable => {
        uri = new URI(stringUri)
        if (uriToNamedBlock.isDefinedAt(uri))
          if (asText == true)
            uriToNamedBlock(uri).associatedComment.map(_.comment).getOrElse("")
          else
            uriToNamedBlock(uri).associatedComment.map(x => new PrettyPrinter(80,2).format(x.toOmdoc)).getOrElse("")
        else
          throw CatalogError("")
      }
    }
  }


  /** Get the text of an entity. The HTTP header also has a field X-Source-url, followed by the position of the entity as returned by getPosition
    * Warning: this function reads the disk, since the text is not stored in memory.
    * @param stringUri URI of a module or URL of a document, given as string
    * @return the actual content, read from the file
    * @throws java.net.URISyntaxException if stringUri does not point to a location on disk which is also stored in memory and, as a URI, it is not valid
    * @throws CatalogError(s: String) if the URI/URL is unknown
    * @throws FileOpenError(s) if the file cannot be read */
  def getText(stringUri : String) : String = {
    val document = new File(stringUri)
    var uri : URI = null
    try {
      // First see whether stringUri points to a file on the disk
      if (document == null)
        throw CatalogError("")
      uri = new URI(getPath(document))
      if (urlToDocument.isDefinedAt(uri)) {
        if (!document.canRead)
          throw FileOpenError("error: file cannot be opened")
        try {
          val source = scala.io.Source.fromFile(document, "utf-8")
          val lines = source.getLines().toArray                          // get all lines from the file
          source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
          return lines.mkString("\n")
        } catch {
          case e : Throwable => throw FileOpenError("error: file cannot be opened or the encoding is not UTF-8")
        }
      }
      else
        throw CatalogError("")
    } catch {   // If it doesn't point to a file on the disk, maybe it's a module URI
      case _ : Throwable => {
        uri = new URI(stringUri)
        if (uriToNamedBlock.isDefinedAt(uri)) {
          val namedBlock = uriToNamedBlock(uri)    // presumably a theory or a view
          val url = namedBlock.url
          val fileUrl = url.getPath
          val pos = namedBlock.pos
          //log(Time + "position: " + pos)
          val doc = new File(fileUrl)
          if (!doc.canRead)
            throw FileOpenError("error: file cannot be opened")
          try {
            val source = scala.io.Source.fromFile(doc, "utf-8")
            val lines = source.getLines().toArray.slice(pos._1._1, pos._2._1 + 1)      // get the desired lines from the file
            //log(Time + lines.mkString("\n"))
            source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
            return lines.mkString("\n").drop(pos._1._2).dropRight(lines.last.length - pos._2._2 - 1)
          } catch {
            case e : Throwable => throw FileOpenError(e.toString + "error: file cannot be opened or the encoding is not UTF-8")
          }
        }
        else
          throw CatalogError("")
      }
    }
  }



  /** Get all dependencies of a module, constant or structure
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an array of dependency URIs, given as strings
    * @throws java.net.URISyntaxException if the URI is not valid
    * @throws CatalogError(s: String) if the URI is unknown */
  def getDependencies(stringUri : String) : Array[String] = {
    val uri = new URI(stringUri)
    if (uriToNamedBlock.isDefinedAt(uri))
        uriToNamedBlock(uri) match {
            case SigBlock(_,_,_,_,deps,_) => deps.toArray.map(_.toString)
            case ViewBlock(_,_,_,_,deps,_,_,_) => deps.toArray.map(_.toString)
            case StrDeclBlock(_,_,_,_,domain,_) => domain.toList.toArray.map(_.toString)
            case _ => Array[String]()
        }
    else
      throw CatalogError("")
  }


   /** Get all children of a namespace URI, module, constant or structure
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an array of children URIs, given as strings
    * @throws java.net.URISyntaxException if the URI is not valid
    * @throws CatalogError(s: String) if the URI is unknown */
  def getChildren(stringUri : String) : Array[String] = {
    val uri = new URI(stringUri)
    if (uriToNamedBlock.isDefinedAt(uri))
        uriToNamedBlock(uri) match {
            case SigBlock(_,_,_,children,_,_) => children.map(_.uri.toString).toArray
            case ViewBlock(_,_,_,children,_,_,_,_) => children.map(_.uri.toString).toArray
            case StrDeclBlock(_,_,_,children,_,_) => children.map(_.uri.toString).toArray
            case _ => Array[String]()
        }
    else
      throw CatalogError("")
  }


  /** Get the position of a module, constant or structure.
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an URL encoding the file and the position within that file
    * @throws java.net.URISyntaxException if the URI is not valid
    * @throws CatalogError(s: String) if the URI is unknown */
  def getPosition(stringUri : String) : String = {
    val uri = new URI(stringUri)
    if (uriToNamedBlock.isDefinedAt(uri))
        uriToNamedBlock(uri).url.toString
    else
      throw CatalogError("")
  }



  /** Get a document skeleton as Omdoc
    * @param stringUrl URL (location on disk) of a document, given as a string
    * @return the document as Omdoc
    * @throws CatalogError(s: String) if the URL is unknown */
  def getOmdoc(stringUrl : String) : String = {
    val document = new File(stringUrl)
    if (document == null)
      throw CatalogError("Invalid URL: " + stringUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document))
    } catch {
      case _ : Throwable => throw CatalogError("Invalid URL: " + stringUrl)
    }
    if (urlToDocument.isDefinedAt(url))
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + new PrettyPrinter(80,2).format(urlToDocument(url).toOmdoc)
    else
      throw CatalogError("Unknown URL: " + stringUrl)
  }


  /** Get all namespaces URIs introduced by a document
    * @param stringUrl URL (location on disk) of a document, given as a string
    * @return an array of namespace URIs given as strings
    * @throws CatalogError(s: String) if the URL is unknown */
  def getNamespaces(stringUrl : String) : Array[String] = {
    val document = new File(stringUrl)
    if (document == null)
      throw CatalogError("Invalid URL: " + stringUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document))
    } catch {
      case _ : Throwable => throw CatalogError("Invalid URL: " + stringUrl)
    }
    if (urlToDocument.isDefinedAt(url))
      urlToDocument(url).declaredNamespaces.toArray.map(_.toString)
    else
      throw CatalogError("Unknown URL: " + stringUrl)
  }


  /** Get the list of namespaces declared in all the files in all the maintained locations
    * @return an array of namespace URIs given as strings */
  def getNamespaces() : Array[String] =
    urlToDocument.values.map(_.declaredNamespaces.map(_.toString))
                 .foldLeft (SortedSet[String]()) ((set, iterable) => set ++ iterable)
                 .toArray


  /** Get the modules introduced by a namespace, in alphabetical order
    * @param stringUri a namespace URI, given as a string
    * @return an URL encoding the file and the position within that file
    * @throws java.net.URISyntaxException if the URI is not valid
    * @throws CatalogError(s: String) if the namespace URI is unknown */
  def getModulesInNamespace(stringUri : String) : Array[String] = {
    val uri = new URI(stringUri)
    if (uriToModulesDeclared.isDefinedAt(uri))
        (SortedSet[String]() ++ uriToModulesDeclared(uri).map(_.toString)).toArray
    else
      throw CatalogError("")
  }


  /** Write the Omdoc skeleton to a file with the same name as the original and in the same folder, but with extension .omdocsk
    * @param stringUrl the URL of the .elf file to be converted.
    * @throws CatalogError(s) if the URL is not OK or not crawled already */
  def writeOmdocToFile(stringUrl : String): Unit = {
    val realUrl : String = java.net.URLDecoder.decode(stringUrl, "UTF-8")
    val document = new File(realUrl)
    if (document == null)
      throw CatalogError("Invalid URL: " + realUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document)).normalize
    } catch {
      case _ : Throwable => throw CatalogError("Invalid URL: " + realUrl)
    }

    var toWrite : String = ""
    if (urlToDocument.isDefinedAt(url))
      toWrite = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + new PrettyPrinter(80,2).format(urlToDocument(url).toOmdoc)
    else
      throw CatalogError("Unknown URL: " + realUrl)

    var newName : String = ""
    if (document.getName.lastIndexOf(".") == -1)
      newName = document.getName + ".omdocsk"
    else
      newName = document.getName.substring(0, document.getName.lastIndexOf(".")) + ".omdocsk"
    val outFile = url.resolve(newName).toString
    var out : BufferedWriter = null

    try {
      out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile),"UTF8"));
    } catch {
      case e : Throwable => { log(Time.toString + outFile + ": error: cannot write to file"); System.exit(3) }
    }
    out.write(toWrite)
    out.close
  }


  // ------------------------------- setter methods -------------------------------


  /** Add an inclusion pattern to the storage */
  def addInclusion(pattern : String): Unit = {
    if (pattern == "")
      log(Time.toString + "error: empty inclusion patern")
    else {
      log(Time.toString + "New inclusion pattern: " + pattern)
      inclusions += pattern
      processedInclusions += quotePattern(pattern)
    }
  }


  /** Add an exclusion pattern to the storage */
  def addExclusion(pattern : String): Unit = {
    if (pattern == "")
      log(Time.toString + "error: empty exclusion patern")
    else {
      log(Time.toString + "New exclusion pattern: " + pattern)
      exclusions += pattern
      processedExclusions += quotePattern(pattern)
    }
  }


  /** Add a location by its string address.
    * If the location is a descendant of an already watched folder, it is not added.
    * If the location is an ancestor of an already watched location, then it is added and the already watched location is removed.
    * @param locationName the (absolute or relative) address of the location on disk
    * @throws InexistentLocation if the file/folder does not exist or cannot be read */
  def addStringLocation(locationName : String): Unit = {
    val location = new File(locationName)
    if (location == null || !location.canRead)
      throw InexistentLocation(getOriginalPath(location) + ": error: location does not exist or cannot be read")

    val path : String = getPath(location)

    if (!isLegalLocation(location.getName, location.isDirectory))
      log(Time.toString + getOriginalPath(location) + ": warning: location is ignored because it does not match the given patterns")
    else {
      // Check whether the location is already watched via an ancestor
      if (locations.exists(f => path.startsWith(getPath(f))))
        log(Time.toString + "Location already watched: " + getOriginalPath(location))
      else {
        log(Time.toString + "New location: " + getOriginalPath(location))
        // Delete descendants from the watch list
        for (f <- locations)
          if (getPath(f).startsWith(path)) {
            log(Time.toString + "Location deleted: " + getOriginalPath(f))
            locations -= f
            log(Time.toString + getOriginalPath(f) + ": uncrawling...")
            uncrawl(getPath(f))
          }
        locations += location
        crawl(location)
      }
    }
  }


  /** Delete from watch list a location given by its string address.
    * @param locationName the (absolute or relative) address of the location on disk
    * @throws InexistentLocation if the location is not in the watch list */
  def deleteStringLocation(locationName : String): Unit = {
    val location = new File(locationName)
    if (location == null || !location.canRead)
      throw InexistentLocation(getOriginalPath(location) + ": error: location does not exist or cannot be read, hence it cannot be deleted")

    if (locations contains location) {
        locations -= location
        uncrawl(getPath(location))
        log(Time.toString + "Location deleted: " + getOriginalPath(location))
    }
    else throw InexistentLocation(getOriginalPath(location) + ": error: location is not in the watch list, hence it cannot be deleted")
  }


  // ------------------------------- crawling -------------------------------


  /** Crawl through all stored locations, ignoring (but logging) errors */
  def crawlAll: Unit = {
    locations.foreach(crawl)
  }


  /** Clear the storage */
  def uncrawlAll: Unit = {
    locations.map(getPath).foreach(uncrawl)
  }


  /** Delete all the hash entries associated with a specific file or folder
    * @param location the file or folder URL, as a string */
  def uncrawl(url: String): Unit = { ConflictGuard.synchronized {
    val crawledFiles = urlToDocument.view.filterKeys(_.toString.startsWith(url)) // the file itself, or all files in the given folder
    for ((url, doc) <- crawledFiles) {
      doc.modules.foreach(m => {
        m match {
            case SigBlock(_,_,_,children,_,_)  => children.foreach(c => c match {
                case CstDeclBlock(uri,_,_,_) => uriToNamedBlock -= uri
                case StrDeclBlock(uri,_,strName,ch,_,_) => {
                    uriToNamedBlock -= uri
                    uriToNamedBlock -= m.uri / strName
                    ch.foreach(cc => uriToNamedBlock -= cc.uri)
                }
            })
            case ViewBlock(_,_,_,children,_,_,_,_) => children.foreach(c => uriToNamedBlock -= c.uri)
        }
        uriToNamedBlock -= m.uri
        val tmp : LinkedHashSet[URI] = uriToModulesDeclared(m.uri.getBase)
        uriToModulesDeclared.update(m.uri.getBase, tmp -- doc.modules.map(_.uri))
      })
      urlToDocument -= url
    }
  }}


  /** Crawl through a specific file or folder
    * @param location the file or folder descriptor */
  def crawl(location: File): Unit = { ConflictGuard.synchronized {
    if (!location.canRead) {
      log(Time.toString + getOriginalPath(location) + ": error: file/folder does not exist or cannot be read")
      return
    }
    val locationName = location.getName    // name without the path

    if (location.isDirectory()) {
      // It's a folder. Crawl it iff it doesn't match any exclusion pattern.
      if (isLegalLocation(locationName, true)) {

          // Get list of children
          var fileList : Array[File] = null
          try {
            fileList = location.listFiles()
          } catch {
            case e : SecurityException => {
              log(Time.toString + getOriginalPath(location) + ": error: folder cannot be read")
              return
            }
          }
          if (fileList == null) {
            log(Time.toString + getOriginalPath(location) + ": error: folder cannot be read")
            return
          }

          // Crawl through the children
          fileList.foreach(crawl)
      }
    }
    // It's a file. Crawl it iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, then all files are crawled.
    else {
      // Check whether this is a recrawl
      val path = new URI(getPath(location))
      if (urlToDocument.contains(path)) {
        if (location.lastModified == urlToDocument(path).lastModified)
          return    // the file was NOT modified after the last crawl
        else {           // remove the file from the hashes first
          log(Time.toString + getOriginalPath(location) + ": uncrawling...")
          uncrawl(path.toString)
        }
      }

      // Crawl the file iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, then all files are crawled.
      if (isLegalLocation(locationName, false)) {
        try {
          // Get the Document instance
          val lastModified = location.lastModified
          var document : Document = null
          try {
              document = FileCrawler(location)          // <-------------------- the actual parsing --------------------
              // prepend the timestamp and file path to each error
              document.errors = document.errors.map(x => ParseError(Time.toString + getOriginalPath(location) + ":" + x))
          } catch {
              case ParseError(s) => {
                if (document == null) {    // then store an empty Document with this error
                    val theError = ParseError(Time.toString + getOriginalPath(location) + ":" + s)
                    document = new Document(new URI(Catalog.getPath(location)), None, new ListBuffer(), new LinkedHashMap(), new LinkedHashSet(), List(theError))
                }
              }
          }
          document.lastModified = lastModified

          // Throw CatalogError if the file has been read before or any component URI is defined elsewhere
          if (urlToDocument.isDefinedAt(document.url))
            throw CatalogError("error: the url of this document (" + document.url + ") has already been encountered elsewhere")

          for (m <- document.modules) {
            if (uriToNamedBlock.isDefinedAt(m.uri))
              throw CatalogError(m.pos.toString + ": error: the uri of this module (" + m.uri + ") has already been encountered at " + getPosition(m.uri.toString))
            m match {
              case SigBlock(_,_,_,children,_,_)  => for (c <- children) if (uriToNamedBlock.isDefinedAt(c.uri))
                throw CatalogError(c.pos.toString + ": error: the uri of this declaration (" + c.uri + ") has already been encountered at " + getPosition(c.uri.toString))
              case ViewBlock(_,_,_,children,_,_,_,_)  => for (c <- children) if (uriToNamedBlock.isDefinedAt(c.uri))
                throw CatalogError(c.pos.toString + ": error: the uri of this assignment (" + c.uri + ") has already been encountered at " + getPosition(c.uri.toString))
            }
          }

          // Update hashes
          urlToDocument.update(document.url, document)
          document.modules.foreach(m => {
              uriToModulesDeclared.update(m.uri.getBase, uriToModulesDeclared.getOrElse(m.uri.getBase, LinkedHashSet()) + m.uri)
              uriToNamedBlock.update(m.uri, m)
              m match {
                  case SigBlock(_,_,_,children,_,_)  => children.foreach(c => c match {
                      case CstDeclBlock(uri,_,_,_) => uriToNamedBlock.update(uri, c)
                      case StrDeclBlock(uri,_,strName,ch,_,_) => {
                          uriToNamedBlock.update(uri, c)
                          uriToNamedBlock.update(m.uri / strName, c)
                          ch.foreach(cc => uriToNamedBlock.update(cc.uri, cc))
                      }
                  })
                  case ViewBlock(_,_,_,children,_,_,_,_) => children.foreach(c => uriToNamedBlock.update(c.uri, c))
          }})

          // Print the parsing errors; if there are none, print "OK"
          if (document.errors.isEmpty)
                log(Time.toString + getOriginalPath(location) + ": OK")
          else
                document.errors.foreach(e => log(e.toString))
        } catch {
          case FileOpenError(s) => log(Time.toString + getOriginalPath(location) + ":" + s)
          case CatalogError(s) => log(Time.toString + getOriginalPath(location) + ":" + s)
          //case e : Exception => log(Time.toString + getOriginalPath(location) + ": error: " + e)
        }
      }
    }
  }}


  // ------------------------------- private methods -------------------------------


  /** Check whether a location matches the stored patterns.
    * @param locationName the file or folder name, excluding its path
    * @param isDirectory set it to true if the location is a directory name, otherwise set it to false
    * @return For directories: true if and only if it doesn't match any exclusion pattern.
              For files: true if and only if it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains. */
  private def isLegalLocation(locationName: String, isDirectory: Boolean) : Boolean =
    if (isDirectory)
      !processedExclusions.exists(locationName.matches)
    else
      (processedInclusions.isEmpty || processedInclusions.exists(locationName.matches)) && !processedExclusions.exists(locationName.matches)


  /** Translates a given pattern into a Java regexp.
    * @param pattern the pattern to be processed, given as a string. The only special character is *, which is interpreted as any sequence of characters.
    * @return a Java regexp pattern with the same meaning as the one given as parameter */
  private def quotePattern(pattern : String) : String = {
    var result = pattern.split("\\Q*\\E").map(Pattern.quote).mkString(".*")
    if (pattern.endsWith("*"))
      result += ".*"
    result
  }
}


/** A thread that checks for updated files and crawls them every crawlingInterval seconds */
class BackgroundCrawler(val catalog: Catalog, val crawlingInterval: Int) extends Thread {
  override def run: Unit = {
    while(true && !isInterrupted()) {
        try {
            Thread.sleep(crawlingInterval * 1000)
        } catch {
            case e: InterruptedException => return
        }
        catalog.crawlAll
    }
  }
}

/** A thread that checks for deleted files every deletingInterval seconds and eliminates them from the hashes */
class BackgroundEliminator(val catalog: Catalog, val deletingInterval: Int) extends Thread {
  override def run: Unit = {
    while(true && !isInterrupted()) {
      try {
          Thread.sleep(deletingInterval * 1000)
      } catch {
           case e: InterruptedException => return
      }
      for (url <- catalog.urlToDocument.keySet) {
        val file = new File(URLDecoder.decode(url.toString, "UTF-8"))  // get the file handle from its disk address
        if (!file.exists) {
          catalog.log(Time.toString + Catalog.getOriginalPath(file) + ": cannot find file. Uncrawling...")
          catalog.uncrawl(url.toString)
        }
      }
    }
  }
}
