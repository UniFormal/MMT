package info.kwarc.mmt.twelf


/** Occurs during file opening */
case class EncodingException(s : String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown when a location specified does not exist on disk */
case class InexistentLocation(s : String) extends Exception(s) {
    override def toString = s
}

/** Exception related to parsing */
case class ParseError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown if a file cannot be opened */
case class FileOpenError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown by the catalog if a query is unsuccessul */
case class CatalogError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown when the web server cannot be started because the specified port is already in use */
case class PortUnavailable(s: String) extends Exception(s) {
    override def toString = s
}


/** Current time as a string */
object Time {
    import java.util.Calendar
    /** Make sure each time component has 2 digits */
    private def formatNumber(n : Int) : String = if (n < 10) ("0" + n.toString) else n.toString
    /** Current time as a string */
    override def toString = {
        val t = Calendar.getInstance
        "[" + formatNumber(t.get(Calendar.HOUR_OF_DAY)) + ":" + formatNumber(t.get(Calendar.MINUTE)) + ":" + formatNumber(t.get(Calendar.SECOND)) + "] "
    }
}


/** For synchronization of update operations */
object ConflictGuard 


/** Checks if a port is used
  * @param port port number to be checked
  * @return true if the port is used, false if it is available */
object isTaken {
    def apply(port: Int) : Boolean = {
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


/** Get a stream to a resource within the JAR file. If it's not found in the JAR, then it looks into the file system
  * @param path to the resource, starting with a slash, e.g. /some-path/resource.txt. 
  * If the resource is in the JAR, then the JAR must look like 
  *       jar-name.jar/some-path/resource.txt
  * If the resource is a separate file and the code is run from outside any JAR, then the folder structure must look like
  *       compiled-folder
  *        |--top-level-package-name/../class-files
  *        |--some-path/resource.txt 
  * @param encoding the encoding of the resource file
  * @return Some(stream to the resource) if found, None otherwise. **The caller must close the stream after reading!**
  * @throws EncodingException(message) if the file is not in the given encoding
  */
object loadResource {
  def apply(path : String, encoding : String) : Option[scala.io.BufferedSource] = {
    val zippedFile = getClass.getResourceAsStream(path)  // the file inside the JAR
    if (zippedFile != null) {
        // Try reading from jar-name.jar/some-path/resource.txt
        try {
          return Some(scala.io.Source.fromInputStream(zippedFile, encoding))
        } catch {
          case _ => throw EncodingException("critical error: " + path + " inside JAR cannot be opened in the " + encoding + " encoding")
        }
    }
    else {
        var filePath : String = null
        try {
          val rootFolder : java.io.File = new java.io.File(getClass.getProtectionDomain.getCodeSource.getLocation.toString)  // e.g. .../lfcatalog/trunk/build/main
          val resourceFolder : String = rootFolder.getParentFile.getParentFile.toString + "/resources"  // e.g. .../lfcatalog/trunk/resources
          filePath = (if (resourceFolder.startsWith("file:")) resourceFolder.substring("file:".length) else resourceFolder) + path
        }
        catch {
          case _ => return None
        }
        val file = new java.io.File(filePath)  // the file on disk
        if (file.exists) {
          // Try reading from compiled-folder/some-path/resource.txt
          val inputStream = new java.io.FileInputStream(file)
          try {
            return Some(scala.io.Source.fromInputStream(inputStream, encoding))
          } catch {
            case _ => throw EncodingException("critical error: " + filePath + " inside JAR cannot be opened in the " + encoding + " encoding")
          }
        }
        else
          return None
    }
  }
}