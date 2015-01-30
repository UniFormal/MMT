package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.collection.mutable.{ListMap,LinkedHashMap}

// redundant at the moment, but we might use it later

/**
 * maintains all errors produced while running [[BuildTarget]]s on an archive
 */
class ErrorManager(archive: WritableArchive) {
   private val errors = new ListMap[String, ErrorMap]
   /**
    * @param key the key of the build target
    * @return the errors of the build target
    * 
    * returns a
    */
   def apply(key: String): ErrorMap = errors.getOrElseUpdate(key, new ErrorMap)
}

/**
 * maintains the errors of a single [[BuildTarget]]
 * 
 * this maps the [[BuildTask]].inPath of a folder/file in the archive to an [[ErrorContainer]]
 */
class ErrorMap extends LinkedHashMap[List[String], ErrorContainer]
