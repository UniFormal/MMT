package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._
import scala.Console._

object Synthesizer {

   val uomstart = "  // UOM start "
   val uomend = "  // UOM end"

   private def getCode(in : BufferedReader) : String = {
     var line = in.readLine
     if (line == null)
       throw new Exception("Synthesizer: No closing //UOM end tag found") 
     if (line.startsWith(uomend)) {
       return ""
     }
     return line + getCode(in)
   }

   def getSnippets(in : BufferedReader, base: Path) : List[(GlobalName, String)] = {
     var line : String = null

     line = in.readLine
     if (line == null) // finished reading file
       return Nil
     if (line.startsWith(uomstart)) {
       line = line.substring(uomstart.length()) // remove the UOM start tag
       return (Path.parseS(line, base), getCode(in))::getSnippets(in, base)
     }
     return getSnippets(in, base)
   }

   def doDocument(controller: Controller, dpath: DPath, scalaFile: File) {
	   val out = new BufferedReader(new FileReader(scalaFile)) 
	   val snippets = getSnippets(out, dpath)
      out.close

	   snippets foreach {
	  	 case (path,code) =>
	  	   val oldcons : Constant = controller.library.get(path) match {
           case cons : Constant => cons
           case _ => throw new Exception(
             "Synthesizer: Path does not point to a constant"
           )
         }
	  	   val newcons = new Constant(oldcons.home, oldcons.name, oldcons.tp, Some(OMFOREIGN(scala.xml.Text(code))), None, None)
	  	   controller.library.update(newcons)
	   }
   }
}