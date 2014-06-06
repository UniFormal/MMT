package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import scala.collection.mutable.{ListMap,LinkedHashMap}

class ErrorManager(archive: WritableArchive) {
   private val errors = new ListMap[String, LinkedHashMap[List[String], ErrorContainer]]
   def apply(key: String) = errors(key)
   def add(key: String) {
      errors(key) = new LinkedHashMap[List[String], ErrorContainer]
   }
}