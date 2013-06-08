package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import metadata._
import documents._
import modules._
import symbols._
import objects._

/** 
 * A parser component for the keywords 'meta' and 'link' to be parsed into the corresponding MetaDatum classes
 * 
 * The parse results are added directly to the containing element 
 */
//TODO this could parse arbitrarily many additional keywords, e.g., @author, @param, and so on
object MetaDataParser extends InDocOrModuleParser {
   def parse(sp: StructureParser, s: ParserState, se: StructuralElement, k: String) {
      val key = sp.readSPath(MetaDatum.keyBase)(s)
      val md = k match {
         case "tag" =>
            Tag(key)
         case "meta" =>
            val (_,_,value) = sp.readParsedObject(OMMOD(MetaDatum.keyBase))(s)
            new MetaDatum(key,value)
         case "link" =>
            val (u,reg) = s.reader.readToken
            val value = utils.URI(u)
            Link(key,value)
      }
      se.metadata.add(md)
   }
}