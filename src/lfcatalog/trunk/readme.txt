-------------------------------
How to run
-------------------------------

Usage: java -jar lfcatalog.jar (--port <port>)? (location|+inclusion|-exclusion)*
  
  location === absolute path to a file or directory
  inclusion === name
  exclusion === name
  name === file or directory name pattern, without its path. Star is the only special character and matches any sequence of characters.
  
  A folder is crawled iff it doesn't match any exclusion pattern.
  A file is crawled iff it matches at least one inclusion pattern, but no exclusion pattern. However, if no inclusion patterns are provided, only the second condition remains.
  The default port is 8080 on localhost.
  

-------------------------------
Command-line user input
-------------------------------

absolute-disk-path          // add a location
delete absolute-disk-path   // delete a location
errors                      // print all files with errors, together with the last error message for each
exit                        // exit the server


-------------------------------
HTTP get server commands
-------------------------------

-------               -----                  ------
Request               Query                  Effect
-------               -----                  ------
 <none>                                 print this readme
 help                                   print this readme
 admin                                  print the admin page
 admin      ?      addLocation=         add a disk location to the list of watched locations
 admin      ?      addInclusion=        add an inclusion pattern (with * replacing any 
                                        sequence of chars)
 admin      ?      addExclusion=        add an exclusion pattern (with * replacing any 
                                        sequence of chars)
 crawlAll                               crawl all files that have been modified since last check
 exit                                   shutdown the server
 
 getMetaText  ?    uri=                 print the semantic comment associated with the entity 
                                        given by URI/file location
 getMeta      ?    uri=                 print the XML representation of the semantic comment 
                                        associated with the entity given by the URI/file location
 getText      ?    uri=                 retrieve the entity from disk (given by URI/file location)
 getDependencies ? uri=                 print the dependencies of the entity given by URI
 getChildren     ? uri=                 print the children of the entity given by URI
 getPosition  ?    uri=                 print the file and position within file of the entity
                                        given by URI
 getPositionInHeader ? uri=             print, in the HTTP header, the file and position within
                                        file of the entity given by URI
 getOmdoc     ?    url=                 print the Omdoc skeleton of the document given by its
                                        disk address
 getNamespaces   ? url=                 print the namespaces introduced by the document given by 
                                        its disk address
 getNamespaces                          print the namespaces introduced by all the documents
 getModules     ? uri=                  print the modules declared in the given namespace

-------------------------------
API (see apidocs for more details)
-------------------------------

// create a Catalog and web server
catalog = new info.kwarc.mmt.lf.Catalog(locations: HashSet[String] = new HashSet(), 
                                        inclusions: HashSet[String] = new HashSet("*.elf"), 
                                        exclusions: HashSet[String] = new HashSet(".svn"), 
                                        port: Int = 8080, 
                                        searchPort: Boolean = false,
                                        log: String => Unit = println,
                                        crawlingInterval: Int = 5, 
                                        deletingInterval: Int = 17)
catalog.init      // throws PortUnavailable if the port is in use

// getters
catalog.queryURI = "http://localhost:"+port+"/getText"
catalog.urlToDocument         : HashMap[URI, Document] 
catalog.uriToNamedBlock       : HashMap[URI, NamedBlock]
catalog.uriToModulesDeclared  : HashMap[URI, LinkedHashSet[URI]]
catalog.getPosition(stringUri : String) : String
catalog.getText(stringUri : String) : String
catalog.getMeta(stringUri : String, asText : Boolean = false) : String
catalog.getDependencies(stringUri : String) : Array[String]
catalog.getChildren(stringUri : String) : Array[String]
catalog.getNamespaces(stringUrl : String) : Array[String]
catalog.getNamespaces : Array[String]
catalog.getModulesInNamespace    : Array[String]

catalog.getOmdoc(stringUrl : String) : String    // only a skeleton
catalog.writeOmdocToFile(stringUrl : String)     // only a skeleton

// setters
catalog.addInclusion(pattern : String)
catalog.addExclusion(pattern : String)
catalog.addStringLocation(locationName : String)
catalog.deleteStringLocation(locationName : String)

// crawling on demand
catalog.crawlAll
catalog.crawl(location: File)
catalog.uncrawl(url: String)

catalog.destroy