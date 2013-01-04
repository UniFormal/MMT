package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._

/*

class ParserState

class StructureParser(controller: Controller) {
   val parserStore = controller.extman.parserStore
   def readModules(r: Reader) {
      val token = r.readToken.getOrElse {
         return
      }
      token._1 match {
         case "theory" =>
            var delim = r.readToken.get
            if (delim._1 == ":") {
               val meta = r.readToken
               delim = r.readToken.get
            }
            if (delim._1 == "=") {
               readDeclarations
            }
         case k =>
            val extParser = parserStore.getParser(k)
            parser(r.readModule)
      }
      readModules(r)
   }
   
}

*/