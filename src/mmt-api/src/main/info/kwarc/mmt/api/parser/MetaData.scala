package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import metadata._
import objects._

/** 
 * A parser component for the keywords 'tag', 'meta', and 'link' to be parsed into the corresponding MetaDatum classes
 * 
 * It also treats various keys starting with @_ as abbreviations of the respective Dublin core keys
 * 
 * The parse results are added directly to the containing element 
 */
object MetadataParser extends ParserExtension {
   private val keywords = List("tag", "link", "meta")
   private val extraKeywords = documents.NarrativeMetadata.allKeys.map("@_"+_)
   def isApplicable(se: StructuralElement, kw: String) = (keywords:::extraKeywords) contains kw 
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, k: String, con:Context = Context.empty) {
      if (extraKeywords contains k) {
         val key = k.substring(2)
         log("found " + key)
         val (value,_) = s.reader.readObject
         (new documents.NarrativeMetadata(key)).update(se, value)
      } else {
         val key = sp.readSPath(s.namespaces.base)(s)
         val md = k match {
            case "tag" =>
               Tag(key)
            case "meta" =>
               val (_,_,value) = sp.readParsedObject(con)(s)
               new MetaDatum(key,value)
            case "link" =>
               val (u,reg) = s.reader.readAll
               val value = s.namespaces.resolve(u)
               Link(key,value)
         }
         se.metadata.add(md)
      }
   }
}