HTTP get server commands
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