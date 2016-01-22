package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import metadata._
import objects._

/** 
 * A parser component for the keywords 'tag', 'meta', and 'link' to be parsed into the corresponding MetaDatum classes
 * 
 * It also treats various keys starting with @ as abbreviations of the respective Dublin core keys
 * 
 * The parse results are added directly to the containing element 
 */
object MetadataParser extends ParserExtension {
   private val keywords = List("tag", "link", "meta")
   private val extraKeywords = documents.NarrativeMetadata.allKeys.map("@"+_)
   def isApplicable(se: StructuralElement, kw: String) = (keywords:::extraKeywords) contains kw 
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, k: String, con:Context = Context.empty) {
      val md = if (extraKeywords contains k) {
         val key = MetaDatum.keyBase ? k.substring(1)
         val (_,_,value) = sp.readParsedObject(Context(MetaDatum.keyBase))(s)
         new MetaDatum(key,value)
      } else {
         val key = sp.readSPath(MetaDatum.keyBase)(s)
         k match {
            case "tag" =>
               Tag(key)
            case "meta" =>
               val (_,_,value) = sp.readParsedObject(con)(s)
               new MetaDatum(key,value)
            case "link" =>
               val (u,reg) = s.reader.readToken
               val value = utils.URI(u)
               Link(key,value)
         }
      }
      se.metadata.add(md)
   }
}