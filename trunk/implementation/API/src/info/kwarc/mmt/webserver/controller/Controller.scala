package info.kwarc.mmt.webserver.controller
import jomdoc._

object Controller extends {
   val report = new frontend.FileReport(new java.io.File("ombase.log"))
} with frontend.Controller(libraries.NullChecker, report) {
   def start() {
      handle(frontend.ExecFile(new java.io.File("startup.mmt")))
   }
   def doGet(doc : String, mod : String, sym : String, act : String) = {
      val action = frontend.Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, basepath)
      log(action.toString)
      val ret : scala.xml.Node = action match {
         case frontend.DefaultGet(p) => frontend.Respond(p,"").get(this)
         case a : frontend.Respond => a.get(this)
         case a => <error action={a.toString}/>
      }
      log("done")
      ret
   }
   def basepath = base
}