package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._
import scala.Console._

object Integrator {

   val uomstart = "  // UOM start "
   val uomend = "  // UOM end"

   private def getCode(in : BufferedReader) : String = {
     var line = in.readLine
     if (line == null)
       throw new Exception("Synthesizer: No closing //UOM end tag found") 
     if (line.startsWith(uomend)) {
       return ""
     }
     return line + "\n" + getCode(in)
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
   
   def mkVarDecl(arg: String) : VarDecl = {
     var nt = arg.split(":")
     var tp = nt(1) match {
       case "Term" => Scala.symbol("Term")
       case "List[Term]" => OMA(Scala.symbol("List"), List(Scala.symbol("Term")))
     } 
     VarDecl(LocalName(nt(0)), Some(tp), None)
   }
   
   def mkLambda(code: String) : Term = {
     val lp = code.indexOf("(") + 1
     val rp = code.indexOf(")")
     val args = code.substring(lp, rp).replace(" ", "").split(",")
     val body = Scala(code.substring(code.indexOf("{") + 1, code.length - 1))
     var con = Context()
     if (args(0) == "") {
       body
     } else {
       args.foreach(con ++= mkVarDecl(_))
       ScalaLambda(con, body)
     }
   }

   def doModule(controller: Controller, mod: Module, scalaFile: File) {
	   val out = new BufferedReader(new FileReader(scalaFile))
	   val snippets = getSnippets(out, mod.path)
       out.close
      
       def merge(c: Constant, code: String) =
         Constant(c.home, c.name, c.alias, c.tp, Some(mkLambda(code)), c.rl, c.not)
      
	   snippets foreach {
	  	 case (path,code) => 
	  	   controller.globalLookup.get(path) match {
           case oldcons : Constant =>
              val newcons = merge(oldcons, code)
              controller.library.update(newcons)
           case oldass : ConstantAssignment =>
              val newass = merge(oldass.toConstant, code).toConstantAssignment
              controller.library.update(newass)
           case _ =>
         }
	   }
   }
}