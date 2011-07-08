// TO*not*DO: in BackgroundEliminator: also check whether the filename or a directory name between the location and the file is now excluded
// correction: don't allow patterns to be removed. Don't even allow them to be added later via the web interface (security problem).

package info.kwarc.mmt.lf

import scala.collection.mutable.HashSet


/** Run the web server and console input */
object Run {
  private val usage = """Usage: java -jar lfcatalog.jar (--port <port>)? (location|+inclusion|-exclusion)*
  
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
  
  def main(args : Array[String]) {
    
    // parse program arguments
    var patternsAndLocations : Array[String] = args
    val locations = new HashSet[String] ()
    val inclusions = new HashSet[String] ()
    val exclusions = new HashSet[String] ()
    if (!args.isEmpty) {      
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
        if (s.startsWith("-"))      // an exclusion pattern
          exclusions += s.drop(1)
        else if (s.startsWith("+")) // an inclusion pattern
          inclusions += s.drop(1)
        else locations += s         // a location
    }
    
    // the main storage and controller (this also starts background threads)
    var catalog = new Catalog(locations, inclusions, exclusions, port, crawlingInterval, deletingInterval)
    try {
        catalog.init
    } catch {
        case PortUnavailable(s) => {
            println("error: port " + port + " is already used. Choose a different port number using the --port argument")
            exit(1)
        }
    }
    
    // accept console input
    while (true) {
          val input = Console.readLine.trim
          val words : Array[String] = input.split("\\s")
          if (words.length >= 1)
            if (words(0) == "exit") {       // exit the program
                catalog.destroy             // stop the server
                exit(0)
            }
            else if (words(0) == "delete") {    // delete a location
                if (words.length >= 2)
                    try {
                        catalog.deleteStringLocation(words(1))
                    } catch {
                        case InexistentLocation(msg) => println(Time + msg)
                    }
                else
                    println(Time + "error: delete must be followed by a location address")
            }
            else if (words(0) == "errors")    // print files with errors
                println(catalog.urlToDocument.filter(_._2.lastError.isDefined).toSeq.sortBy(_._1.toString).map(_._2.lastError.get).mkString("\n"))
            else {          // add a location
                try {
                    catalog.addStringLocation(words(0))
                } catch {
                    case InexistentLocation(msg) => println(Time + msg)
                }
            }
    }
  }
}