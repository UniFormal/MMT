package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._

class PythonExporter extends Exporter {
   override val outExt = "py"
   val key = "python"
   override val folderName = "__init__"

   private def comment(body: => Unit) {
      rh("# ")
      body
      rh.nl
   }
   private def cls(name: String)(body: => Unit) {
      rh("class " + name + ":")
      rh.indent(body)
      rh.nl
   }
   private def df(name: String, vars: List[String])(body: => Unit) {
      rh("def " + name + vars.mkString("(", ", ", ")") + ":")
      rh.indent(body)
      rh.nl
   }

   def exportTheory(t: DeclaredTheory, bf: BuildTask) {
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

   def exportView(v: DeclaredView, bf: BuildTask) {}

   def exportNamespace(dpath: DPath, nsbt: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
         namespaces.foreach {case bd =>
            comment {rh("package " + bd.dirName)}
         }
         modules.foreach {case bf =>
            comment {rh("module " + bf.outFile.segments.last)}
         }
   }

   def exportDocument(doc: Document, bt: BuildTask) {

   }
}
