package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._

class PythonExporter extends ContentExporter with IndentedExporter {
   val outDim = "python"
   override val outExt = "py"
   val key = "python"
   override val folderName = "__init__"

   private def comment(body: => Unit) {
      rh("# ")
      afterIndentationString += "# "
      body
      afterIndentationString = afterIndentationString.substring(0, afterIndentationString.length-2)
      nl
   }
   private def cls(name: String)(body: => Unit) {
      rh("class " + name + ":")
      indent(body)
      nl
   }
   private def df(name: String, vars: List[String])(body: => Unit) {
      rh("def " + name + vars.mkString("(", ", ", ")") + ":")
      indent(body)
      nl
   }
   
   def doTheory(t: DeclaredTheory) {
      cls(t.name.toPath) {
         var fields: List[String] = Nil
         t.getPrimitiveDeclarations.foreach {
            case c: Constant =>
               c.not match {
                  case Some(n) =>
                     
                  case None => 0
               }
               df(c.name.toPath, Nil){
                  rh("pass")
                  fields ::= c.name.toPath
               }
            case _ =>
         }
         fields
      }
   }
   
   def doView(v: DeclaredView) {}
   
   def doNamespace(dpath: DPath, namespaces: List[(BuiltDir,DPath)], modules: List[(BuiltFile,MPath)]) {
         namespaces.foreach {case (bd, dp) =>
            comment {rh("package " + bd.dirName)}
         }
         modules.foreach {case (bf, mp) =>
            comment {rh("module " + bf.outFile.segments.last)}
         }
   }
}