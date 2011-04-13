package info.kwarc.mmt.web.snippet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.BindHelpers._
import info.kwarc.mmt.web.controller._
import info.kwarc.mmt.api.frontend._

class Admin {
   def exec : scala.xml.NodeSeq  = {
      val a = S.param("command").getOrElse("") match {
        case "" => NoAction
        case s => Action.parseAct(s, Controller.basepath)
      }
      a match {
         case Clear =>
           Controller.clear
           <p>clear: OK</p>
         case PrintAll => scala.xml.Text(Controller.library.toString)
         case Exit => exit
         case NoAction => <p>OK</p>
         case _ => <p>unsupported command</p>
      }
   }
   
   private def available = List(("clear", "clear"), ("printAll", "print all"))
   def commands(xhtml : scala.xml.NodeSeq) : scala.xml.NodeSeq = {
      available.flatMap({case (t,d) => BindHelpers.bind("i", xhtml,
         AttrBindParam("target", scala.xml.Text(":admin?" + t), "href"),
         "description" -> d)
      })
   }
}
