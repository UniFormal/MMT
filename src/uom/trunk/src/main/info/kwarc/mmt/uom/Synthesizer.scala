package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._
import scala.Console._

object Synthesizer extends {
   val report = new frontend.FileReport(new java.io.File("uom.log"))
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)
   } with Controller(checker, report) {

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

   def getSnippets(in : BufferedReader) : List[(GlobalName, String)] = {
     var line : String = null

     line = in.readLine
     if (line == null) // finished reading file
       return Nil
     if (line.startsWith(uomstart)) {
       line = line.substring(uomstart.length()) // remove the UOM start tag
       return (Path.parseS(line, base), getCode(in))::getSnippets(in)
     }
     return getSnippets(in)
   }

   def main(args: Array[String]) {
     /* location of the scala file  */
	   val scalafile = new BufferedReader(new FileReader(args(0))) 
	   val snippets = getSnippets(scalafile)
     scalafile.close

     val dpath = read(new java.io.File(args(1)))

	   snippets foreach {
	  	 case (path,code) =>
	  	   val oldcons : Constant = library.get(path) match {
           case cons : Constant => cons
           case _ => throw new Exception(
             "Synthesizer: Path does not point to a constant"
           )
         }
	  	   val newcons = new Constant(oldcons.home, oldcons.name, oldcons.tp, 
           Some(OMFOREIGN(scala.xml.Text(code))), null) 
	  	   library.update(newcons)
	   }
     val doc = getDocument(dpath)

     var out = new PrintWriter(new BufferedWriter(new FileWriter(args(1), false))); // open and overwrite
     out.println(doc.toNodeResolved(library))
     out.close
   }
}

