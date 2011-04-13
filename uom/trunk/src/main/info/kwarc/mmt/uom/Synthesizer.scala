package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._

object Synthesizer extends {
   val report = new frontend.FileReport(new java.io.File("uom.log"))
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)} with Controller(checker, report) {

   val uomstart = "  // UOM start "
   val uomend = "  // UOM end"

   private def getCode(in : BufferedReader) : String = {
     var line = in.readLine
     //println(line)
     if (line == null)
      throw new java.lang.Exception("Synthesizer: No closing //UOM end tag found") 
     if (line.startsWith(uomend)) {
       //println("Found closing tag")
       return ""
     }
     return line + getCode(in)
   }

   def getSnippets(in : BufferedReader) : List[(GlobalName, String)] = {
	   // Path.parseS(string): SPath
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
	   val scalafile = new BufferedReader(new FileReader(args(0))) // location of scala file
	   val snippets = getSnippets(scalafile)
      scalafile.close

	   handle(Local)
	   val doc = DPath(utils.xml.URI(new java.io.File(args(1)).toURI)) // physical location of the file
	   handle(Read(doc))

//     val dpath = Path.parseD(args(2), base) // semantic identifier of the document (given by base attribute)

      println(library.toString)
	   snippets foreach {
	  	   case (path,code) =>
           println("Path : " + path.toString + "\n\nCode : \n\n" + code +"\n\n")
	  	     val oldcons : Constant = library.get(path) match {
             case cons : Constant => cons
             case _ => throw new java.lang.Exception("Synthesizer: Path does not point to a constant")
           }
	  	     val newcons = new Constant(oldcons.home, oldcons.name, oldcons.tp, Some(OMFOREIGN(scala.xml.Text(code))), null) 
	  	     library.update(newcons)
	   }
	   println(library.toString)
//	   val doc = try {docstore.get(dpath)} // get the content of the document as a list of reference elements
//	             catch {case jomdoc.frontend.NotFound(p) => println(p.toPath + " not found"); exit}
//	   println(doc.toString)
   }
}

