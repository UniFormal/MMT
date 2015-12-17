package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import web._
import utils._
import parser._
import symbols._
import objects._

object MMTTeX {
   def mmtCurrent(id: Int) = s"\\csname mmt@${id.toString}\\endcsname"
}

import MMTTeX._

class MMTTeX extends ServerExtension("mmttex") {

   def apply(path: List[String], query: String, body: Body) = {
      var response = ""
      val f = File(query)
      val ps = ParsingStream.fromFile(f, formatOpt = Some("mmt"))
      val ec = new ErrorContainer(None)
      val doc = controller.read(ps, true)(ec)
      val mods = doc.getModulesResolved(controller.globalLookup)
      val op = controller.extman.get(classOf[LatexObjectPresenter]).headOption.getOrElse {
         val op = new LatexObjectPresenter()
         controller.extman.addExtension(op) // make sure it's initialized
         op
      }
      def doModule(m: ContentElement) { 
         m.getDeclarations.foreach {
            case c: Constant =>
               val name = c.name.toPath
               if (name.startsWith("mmt@") && c.df.isDefined) {
                  val id = name.substring(4).toInt
                  val df = c.df.get
                  val dfL = op.asString(df, Some(c.path $ DefComponent))
                  response += s"\\expandafter\\def${mmtCurrent(id)}{$dfL}\n"
               }
            case nm: NestedModule =>
               doModule(nm.module)
            case _ =>
         }
      }
      mods foreach doModule
      File.write(f.addExtension("sty"), response)
      Server.TextResponse(response)
   }
}