package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._
import scala.Console._
import scala.tools.nsc.util._
 
object ScalaReflection {
  implicit def any2anyExtras(x: Any) = new AnyExtras(x)
}
 
class AnyExtras(x: Any) {
  def methods_ = println(methods.reduceLeft[String](_ + ", " + _))
  def methods__ = methods.foreach(println _)
  def fields_ = println(fields.reduceLeft[String](_ + ", " + _))
  def fields__ = fields.foreach(println _)
 
  def methods = wrapped.getClass
      .getDeclaredMethods
      .toList
      .map(m => m.toString
                        .replaceFirst("\\).*", ")")
                        .replaceAll("[^(]+\\.", "")
                        .replace("()", ""))
      .filter(!_.startsWith("$tag"))
 
  def fields = wrapped.getClass
      .getDeclaredFields
      .toList
      .map(m => m.toString.replaceFirst("^.*\\.", ""))
 
  private def wrapped: AnyRef = x match {
    case x: Byte => byte2Byte(x)
    case x: Short => short2Short(x)
    case x: Char => char2Character(x)
    case x: Int => int2Integer(x)
    case x: Long => long2Long(x)
    case x: Float => float2Float(x)
    case x: Double => double2Double(x)
    case x: Boolean => boolean2Boolean(x)
    case _ => x.asInstanceOf[AnyRef]
  }
}



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

//   println(library.toString)
	   snippets foreach {
	  	   case (path,code) =>
           println("Path : " + path.toString + "\n\nCode : \n\n" + code +"\n\n")
	  	     val oldcons : Constant = library.get(path) match {
             case cons : Constant => cons
             case _ => throw new java.lang.Exception("Synthesizer: Path does not point to a constant")
           }
	  	     val newcons = new Constant(oldcons.home, oldcons.name, oldcons.tp, Some(OMFOREIGN(scala.xml.Text(code))), null) 
	  	     library.update(newcons)
//           println(new AnyExtras(library).methods__)
	   }
	   //println(library.toString)
//	   val doc = try {docstore.get(dpath)} // get the content of the document as a list of reference elements
//	             catch {case jomdoc.frontend.NotFound(p) => println(p.toPath + " not found"); exit}
//	   println(doc.toString)
   }
}

