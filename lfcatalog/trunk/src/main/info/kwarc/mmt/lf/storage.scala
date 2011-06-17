// TODO: see how to delete dependencies, etc, when a file disappears or no longer compiles: simply delete all the things associated with that file

package info.kwarc.mmt.lf

import scala.collection.mutable.{HashSet, LinkedHashSet, HashMap}
import java.io._
import java.util.regex.Pattern
import scala.xml._
import java.net._

/** */
case class InexistentLocation(s : String) extends Exception(s)
case class ParseError(s: String) extends Exception(s)
case class FileOpenError(s: String) extends Exception(s)
case class StorageError(s: String) extends Exception(s)


/** Get the physical path to a file or folder, NOT %-encoded
  * @param location the file descriptor
  * @return the path as a string */
object getOriginalPath {
  def apply(location: File) : String = location.toURI.normalize.getPath
}


/** Get the physical path to a file or folder, %-encoded
  * @param location the file descriptor
  * @return the path as a string */
object getPath {
  def apply(location: File) : String = location.toURI.normalize.getRawPath
}


/** Object containing the information obtained by crawling all the locations */
object Storage {
  
  /** Get the semantic comment associated with a document, module, constant or structure
    * @param stringUri URI of a module or URL of a document, given as string
    * @param asText set it to true for text output and to false for XML output
    * @return if asText is true, the semantic comment, copied character by character from the file. Otherwise, an XML representation of the comment
    * @throws java.net.URISyntaxException if stringUri does not point to a location on disk and, as a URI, it is not valid 
    * @throws StorageError(s: String) if the URI/URL is unknown */
  def getMeta(stringUri : String, asText : Boolean = false) : String = {
    val document = new File(stringUri)
    var uri : URI = null
    try {
      // First see whether stringUri points to a file on the disk
      if (document == null)
        throw StorageError("")
      uri = new URI(getPath(document))
      if (urlToDocument.isDefinedAt(uri))
        if (asText == true)
          urlToDocument(uri).associatedComment.map(_.comment).getOrElse("")
        else
          urlToDocument(uri).associatedComment.map(x => new PrettyPrinter(80,2).format(x.toOmdoc)).getOrElse("")
      else
        throw StorageError("")
    } catch {   // If it doesn't point to a file on the disk, maybe it's a module URI
      case _ => {
        uri = new URI(stringUri) 
        if (uriToNamedBlock.isDefinedAt(uri))
          if (asText == true)
            uriToNamedBlock(uri).associatedComment.map(_.comment).getOrElse("")
          else
            uriToNamedBlock(uri).associatedComment.map(x => new PrettyPrinter(80,2).format(x.toOmdoc)).getOrElse("")
        else
          throw StorageError("")
      }
    }
  }
  
  
  /** Get the text of an entity. Warning: this function reads the disk, since the text is not stored in memory.
    * @param stringUri URI of a module or URL of a document, given as string
    * @return the actual content, read from the file
    * @throws java.net.URISyntaxException if stringUri does not point to a location on disk which is also stored in memory and, as a URI, it is not valid 
    * @throws StorageError(s: String) if the URI/URL is unknown
    * @throws FileOpenError(s) if the file cannot be read */
  def getText(stringUri : String) : String = {
    val document = new File(stringUri)
    var uri : URI = null
    try {
      // First see whether stringUri points to a file on the disk
      if (document == null)
        throw StorageError("")
      uri = new URI(getPath(document))
      if (urlToDocument.isDefinedAt(uri)) {
        if (!document.canRead)
          throw FileOpenError("error: file cannot be opened")
        try {
          val source = scala.io.Source.fromFile(document, "utf-8")
          val lines = source.getLines.toArray                          // get all lines from the file
          source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
          return lines.mkString("\n")
        } catch {
          case e => throw FileOpenError("error: file cannot be opened or the encoding is not UTF-8")
        } 
      }
      else
        throw StorageError("")
    } catch {   // If it doesn't point to a file on the disk, maybe it's a module URI
      case _ => {
        uri = new URI(stringUri) 
        if (uriToNamedBlock.isDefinedAt(uri)) {
          val namedBlock = uriToNamedBlock(uri)    // presumably a theory or a view
          val url = namedBlock.url
          val fileUrl = url.getPath
          val pos = namedBlock.pos
          //println("position: " + pos)
          val doc = new File(fileUrl)
          if (!doc.canRead)
            throw FileOpenError("error: file cannot be opened")
          try {
            val source = scala.io.Source.fromFile(doc, "utf-8")
            val lines = source.getLines.toArray.slice(pos._1._1, pos._2._1 + 1)      // get the desired lines from the file
            //println(lines.mkString("\n"))
            source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
            return lines.mkString("\n").drop(pos._1._2).dropRight(lines.last.length - pos._2._2 - 1)
          } catch {
            case e => throw FileOpenError(e + "error: file cannot be opened or the encoding is not UTF-8")
          }
        }
        else
          throw StorageError("")
      }
    }
  }
  
  
  
  /** Get all dependencies of a module, constant or structure
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an array of dependency URIs, given as strings
    * @throws java.net.URISyntaxException if the URI is not valid 
    * @throws StorageError(s: String) if the URI is unknown */
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
      throw StorageError("")
  }
  
  
   /** Get all children of a namespace URI, module, constant or structure
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an array of children URIs, given as strings
    * @throws java.net.URISyntaxException if the URI is not valid 
    * @throws StorageError(s: String) if the URI is unknown */
  def getChildren(stringUri : String) : Array[String] = {
    val uri = new URI(stringUri)
    if (uriToNamedBlock.isDefinedAt(uri))
        uriToNamedBlock(uri) match {
            case SigBlock(_,_,_,_,deps,_) => deps.toArray.map(_.toString)
            case ViewBlock(_,_,_,_,deps,_,_,_) => deps.toArray.map(_.toString)
            case StrDeclBlock(_,_,_,_,domain,_) => domain.toList.toArray.map(_.toString)
            case _ => Array[String]()
        }
    else
      throw StorageError("")
  }
  
  
  /** Get the position of a module, constant or structure
    * @param stringUri URI of a module / constant / structure, given as a string
    * @return an URL encoding the file and the position within that file
    * @throws java.net.URISyntaxException if the URI is not valid 
    * @throws StorageError(s: String) if the URI is unknown */
  def getPosition(stringUri : String) : String = {
    val uri = new URI(stringUri)
    if (uriToNamedBlock.isDefinedAt(uri))
        uriToNamedBlock(uri).url.toString
    else
      throw StorageError("")
  }
  
  
  
  /** Get a document skeleton as Omdoc
    * @param stringUrl URL (location on disk) of a document, given as a string
    * @return the document as Omdoc
    * @throws StorageError(s: String) if the URL is unknown */
  def getOmdoc(stringUrl : String) : String = {
    val document = new File(stringUrl)
    if (document == null)
      throw StorageError("Invalid URL: " + stringUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document))
    } catch {
      case _ => throw StorageError("Invalid URL: " + stringUrl)
    }
    if (urlToDocument.isDefinedAt(url))
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + new PrettyPrinter(80,2).format(urlToDocument(url).toOmdoc)
    else
      throw StorageError("Unknown URL: " + stringUrl)
  }
  
  
  /** Get all namespaces URIs introduced by a document
    * @param stringUrl URL (location on disk) of a document, given as a string
    * @return an array of module URIs introduced, given as strings
    * @throws StorageError(s: String) if the URL is unknown */
  def getNSIntroduced(stringUrl : String) : Array[String] = {
    val document = new File(stringUrl)
    if (document == null)
      throw StorageError("Invalid URL: " + stringUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document))
    } catch {
      case _ => throw StorageError("Invalid URL: " + stringUrl)
    }
    if (urlToDocument.isDefinedAt(url))
      urlToDocument(url).declaredNamespaces.toArray.map(_.toString)
    else
      throw StorageError("Unknown URL: " + stringUrl)
  }
  
  
  /** Write the Omdoc skeleton to a file with the same name as the original and in the same folder, but with extension .omdocsk
    * @param stringUrl the URL of the .elf file to be converted.
    * @throws StorageError(s) if the URL is not OK or not crawled already */
  def writeOmdocToFile(stringUrl : String) {
    val realUrl : String = java.net.URLDecoder.decode(stringUrl, "UTF-8")
    val document = new File(realUrl)
    if (document == null)
      throw StorageError("Invalid URL: " + realUrl)
    var url : URI = null
    try {
      url = new URI(getPath(document)).normalize
    } catch {
      case _ => throw StorageError("Invalid URL: " + realUrl)
    }
    
    var toWrite : String = ""
    if (urlToDocument.isDefinedAt(url))
      toWrite = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + new PrettyPrinter(80,2).format(urlToDocument(url).toOmdoc)
    else
      throw StorageError("Unknown URL: " + realUrl)
    
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
      case e => { println(outFile + ": error: cannot write to file"); System.exit(3) }
    }
    out.write(toWrite)
    out.close
  }
  
  
  /** Add an inclusion pattern to the storage */
  def addInclusion(pattern : String) {
    if (pattern.isEmpty)
      println("error: empty inclusion patern")
    else {
      println("New inclusion pattern: " + pattern)
      inclusions += pattern
      processedInclusions += quotePattern(pattern)
    }
  }
  
  
  /** Add an exclusion pattern to the storage */
  def addExclusion(pattern : String) {
    if (pattern.isEmpty)
      println("error: empty exclusion patern")
    else {
      println("New exclusion pattern: " + pattern)
      exclusions += pattern
      processedExclusions += quotePattern(pattern)
    }
  }
  
  /** Exclusion patterns for files and folders. 
    * Star is the only special character and matches any sequence of characters. 
    * A folder is crawled iff it doesn't match any exclusion pattern.
    * A file is crawled iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains. */
  def getExclusions = exclusions
  
  /** Inclusion patterns for files */
  def getInclusions = inclusions
  
  /** Exclusion patterns for files and folders. */
  private val exclusions = HashSet[String] ()
  
  /** Inclusion patterns for files */
  private val inclusions = HashSet[String] ()
  
  
  /** Processed exclusion and inclusion patterns (for internal use)
    * Everything is quoted, except *, which are replaced with .* 
    */
  private val processedExclusions = HashSet[String] ()
  private val processedInclusions = HashSet[String] ()
  
  
  /** Locations (files and folders) being watched */
  val locations = HashSet[File] ()
  
  /** Map from URIs to named blocks (modules, declarations or assignments) */
  val uriToNamedBlock = HashMap[URI, NamedBlock] ()
  
  /** Map from URLs to documents */
  val urlToDocument = HashMap[URI, Document] ()
  
  /** Map from namespace URIs to modules declared in that URI */
  val uriToModulesDeclared = HashMap[URI, LinkedHashSet[URI]] ()
  
  
  /** Add a location by its string address.
    * If the location is a descendant of an already watched folder, it is not added.
    * If the location is an ancestor of an already watched location, then it is added and the already watched location is removed.
    * @param locationName the (absolute or relative) address of the location on disk
    * @throws InexistentLocation if the file/folder does not exist or cannot be read */
  def addStringLocation(locationName : String) {
    val location = new File(locationName)
    if (location == null || !location.canRead)
      throw InexistentLocation(getOriginalPath(location) + ": error: location does not exist or cannot be read")
      
    val path : String = getPath(location)
    
    if (!isLegalLocation(location.getName, location.isDirectory))
      println(getOriginalPath(location) + ": warning: location is ignored because it does not match the given patterns")
    else {
      // Check whether the location is already watched via an ancestor
      if (locations.exists(f => path.startsWith(getPath(f))))
        println("Location already watched: " + getOriginalPath(location))
      else {
        println("New location: " + getOriginalPath(location))
        // Delete descendants from the watch list
        for (f <- locations)
          if (getPath(f).startsWith(path)) {
            println("Location deleted: " + getOriginalPath(f))
            locations -= f
            println(getOriginalPath(f) + ": uncrawling...")
            uncrawl(getPath(f))
          }
        locations += location
        crawl(location)
      }
    }
  }
  
  
  
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
  
  
  /** Crawl through all stored locations, ignoring (but logging) errors */
  def crawlAll { ConflictGuard.synchronized {
    locations.foreach(crawl)
  }}
  
  
  /** Empty all the hashes */
  def emptyAll { ConflictGuard.synchronized { 
    uriToNamedBlock.clear
    urlToDocument.clear
  }}
  
  
  /** Delete all the hash entries associated with a specific file or folder
    * @param location the file or folder URL, as a string */
  def uncrawl(url: String) { ConflictGuard.synchronized { 
    val crawledFiles = urlToDocument.filterKeys(_.toString.startsWith(url))
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
  def crawl(location: File) { ConflictGuard.synchronized { 
    if (!location.canRead) {
      println(getOriginalPath(location) + ": error: file/folder does not exist or cannot be read")
      return
    }
    val locationName = location.getName    // name without the path
    
    if (location.isDirectory()) {
      // Crawl the folder iff it doesn't match any exclusion pattern.
      if (isLegalLocation(locationName, true)) {
          //println(getOriginalPath(location) + ": crawling folder...")
        
          // Get list of children
          var fileList : Array[File] = null
          try {
            fileList = location.listFiles()
          } catch {
            case e : SecurityException => { 
              println(getOriginalPath(location) + ": error: folder cannot be read")
              return
            }
          }
          if (fileList == null) {
            println(getOriginalPath(location) + ": error: folder cannot be read")
            return
          }
          
          // Crawl through the children
          fileList.foreach(crawl)
      }
    }
    else {
      // Check whether this is a recrawl
      val path = new URI(getPath(location))
      if (urlToDocument.contains(path)) {
        if (location.lastModified == urlToDocument(path).lastModified)
          return                      // the file was NOT modified after the last crawl
        else {           // remove the file from the hashes first
          println(getOriginalPath(location) + ": uncrawling...")
          uncrawl(path.toString)
        }
      }
        
      // Crawl the file iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, then all files are crawled.
      if (isLegalLocation(locationName, false)) {
        println(getOriginalPath(location) + ": crawling file...")
        try {
          // Crawl the file to get the document structure
          val lastModified = location.lastModified
          val document = FileCrawler(location)
          document.lastModified = lastModified
          
          // Check whether anything has been already read
          if (urlToDocument.isDefinedAt(document.url))
            throw StorageError("error: the url of this document (" + document.url + ") has already been encountered elsewhere")
            
          for (m <- document.modules) {
            if (uriToNamedBlock.isDefinedAt(m.uri))
              throw StorageError(m.pos + ": error: the uri of this module (" + m.uri + ") has already been encountered elsewhere")
            m match {
              case SigBlock(_,_,_,children,_,_)  => for (c <- children) if (uriToNamedBlock.isDefinedAt(c.uri))
                throw StorageError(c.pos + ": error: the uri of this declaration (" + c.uri + ") has already been encountered elsewhere")
              case ViewBlock(_,_,_,children,_,_,_,_)  => for (c <- children) if (uriToNamedBlock.isDefinedAt(c.uri))
                throw StorageError(c.pos + ": error: the uri of this assignment (" + c.uri + ") has already been encountered elsewhere")
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
            
          println(getOriginalPath(location) + ": success")
        } catch {
          case ParseError(s) => println(getOriginalPath(location) + ":" + s)
          case FileOpenError(s) => println(getOriginalPath(location) + ":" + s)
          case StorageError(s) => println(getOriginalPath(location) + ":" + s)
          case e : Exception => println(getOriginalPath(location) + ": error: " + e)
        }
      }
    }
  }}
}