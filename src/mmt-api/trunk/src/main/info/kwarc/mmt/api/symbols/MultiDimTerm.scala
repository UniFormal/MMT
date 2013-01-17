package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._

class StringDimension {
   var string: Option[String] = None
   var dirty: Boolean = false
}

class TermDimension {
   var term: Option[Term] = None
   var dirty: Boolean = false
}

class MultiDimTerm {
   private val read = new StringDimension
   private val parsed = new TermDimension
   private val reconstructed = new TermDimension
   def read_=(s: String) {
      read.string = Some(s)
      parsed.dirty = true
      reconstructed.dirty = true
   }
   def parsed_=(t: Term) {
      parsed.term = Some(t)
      parsed.dirty = false
      reconstructed.dirty = true
   }
   def reconstructed_=(t: Term) {
      reconstructed.term = Some(t)
      reconstructed.dirty = false
   }
}