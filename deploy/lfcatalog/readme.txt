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
 getOmdoc     ?    url=                 print the Omdoc skeleton of the document given by its
                                        disk address
 getNSIntroduced ? url=                 print the namespaces introduced by the document given by 
                                        its disk address
 