package info.kwarc.mmt.web
import info.kwarc.mmt.api._
import info.kwarc.mmt.uom._

object Manager {
   val report = new frontend.FileReport(new java.io.File("server.log"))
   val controller = new frontend.Controller(libraries.NullChecker, report)
   val uom = new UOMServer(report)
   
   def log(msg: => String) = report("manager", msg)
      
   def start() {
      controller.handle(frontend.ExecFile(new java.io.File("startup.mmt")))
      uom.init
   }
   def doGet(doc : String, mod : String, sym : String, act : String) = {
      val action = frontend.Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, basepath, controller.getHome)
      log(action.toString)
      val ret : scala.xml.Node = action match {
         case frontend.DefaultGet(p) => frontend.Respond(p,"").get(controller)
         case a : frontend.Respond => a.get(controller)
         case a => <error action={a.toString}/>
      }
      log("done")
      ret
   }
   def basepath = controller.getBase
}