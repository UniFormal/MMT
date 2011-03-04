package info.kwarc.mmt.uom.extractor
import jomdoc._
import jomdoc.frontend._
import jomdoc.modules._
import jomdoc.symbols._
import jomdoc.utils.MyList._

object Extractor extends {
   val report = new frontend.FileReport(new java.io.File("jomdoc.log"))
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)} with Controller(checker, report) {
   def doTheory(t: Theory) {
	   t.valueList map {
	      case c: Constant => println("constant " + c.name) // handle constants here
	      case PlainImport(_, from) => println("include from " + from.toPath) // handle includes here
	      // case s: Structure => // TODO later
	      case _ => 
	   }
   }
   def main(args: Array[String]) {
	   handle(Local)
	   val file = utils.xml.URI(new java.io.File(args(0)).toURI) // physical location of the file
	   handle(Read(DPath(file)))
	   val dpath = Path.parseD(args(1), base) // semantic identifier of the document (given by base attribute)
	   
	   val doc = try {docstore.get(dpath)} // get the content of the document as a list of reference elements
	             catch {case jomdoc.frontend.NotFound(p) => println(p.toPath + " not found"); exit}
	   val theos : List[Theory] =  // dereference all and keep the theories
	  	   doc.getItems mapPartial {r => get(r.target) match {
	  	  	   case t : Theory => Some(t)
	  	  	   case _ => None
	  	   }
	   }
	   theos foreach doTheory // handle all theories
   }
}
   
object Synthesizer extends {
   val report = new frontend.FileReport(new java.io.File("jomdoc.log"))
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)} with Controller(checker, report) {

   def getSnippets(scalafile : java.io.File) : List[(SPath, String)] = {
	   // Path.parseS(string): SPath
	   null
   }
   def main(args: Array[String]) {
	   val scalafile = new java.io.File(args(0))
	   val snippets = getSnippets(scalafile)
	   
	   handle(Local)
	   val file = utils.xml.URI(new java.io.File(args(1)).toURI) // physical location of the file
	   handle(Read(DPath(file)))

	   snippets foreach {
	  	   case (path,code) =>
	  	     val oldcons = library.get(path)
	  	     //val newcons = new Constant(oldcons.parent, oldcons.name, oldcons.tp, Some(OMFOREIGN(scala.xml.Text(code))), null, None) 
	  	     library.update(newcons)
	   }
	   val doc = try {docstore.get(dpath)} // get the content of the document as a list of reference elements
	             catch {case jomdoc.frontend.NotFound(p) => println(p.toPath + " not found"); exit}
	   println(doc.toOMDoc)
   }
}