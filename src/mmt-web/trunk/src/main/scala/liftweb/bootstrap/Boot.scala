package bootstrap.liftweb
import info.kwarc.mmt.web._
import info.kwarc.mmt.api._
import frontend._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.common.{Box,Empty,Full}

/**
  * A class that's instantiated early and run. 
  * It allows the application to modify lift's environment.
  */
class Boot {
   /** init method */
   def boot {
     // where to search snippets
     LiftRules.addToPackages("info.kwarc.mmt.web")

     // add URL rewrites
     LiftRules.statelessRewrite.append {
          // empty path defaults to browser interface
          case RewriteRequest(ParsePath(Nil, _, _, _), GetRequest, req) =>
               val initial = ReqHelpers.query(req)
               RewriteResponse(ParsePath(List("xhtml", "browse"), "html", true, false), Map(("query", initial)), false)
          // files making up the user interface
          case RewriteRequest(ParsePath(hd :: tl, s, a, e), GetRequest, request) 
               if List("xhtml", "css", "script") contains hd =>
                  //css and javascript paths cannot be rewritten here because they are served by the container, see fixCSS method 
                  val p = ParsePath(hd :: tl, s, a, e)
                  RewriteResponse(p, Map.empty, true)
          // services making up the machine interface, also used via ajax
          case RewriteRequest(ppath, GetRequest, request) 
               if Rest.applicable(ppath) =>
                  RewriteResponse(ppath, Map.empty, true)
         /* URI of the form CATALOG-AUTHORITY/;?doc?mod?sym?params
          * Ideally: URI of the form CATALOG-AUTHORITY?doc?mod?sym?params
          * But firefox transforms "path" to "/path" if path(0) != "/"
          * so the next best thing to an empty path is the path "/;"
          */
          case RewriteRequest(ParsePath(List(";"), _, _, _), GetRequest, req) =>
               val initial = ReqHelpers.query(req)
               RewriteResponse(ParsePath(List("xhtml","browse"), "html", true, false), Map(("query", initial)), true)
         /* URI of the form OMBASE-AUTHORITY/path?mod?sym?params where doc := OMBASE-AUTHORITY/path
          * lift transforms "" and "/" to "/index" (see net.liftweb.http.Req.parsePath)
          * lift also removes empty path segments
          * path(request) yields the original (client-sent) path
          */
          case RewriteRequest(_, GetRequest, req) =>
               val initial = ReqHelpers.path(req) + "?" + ReqHelpers.query(req)
               RewriteResponse(ParsePath(List("xhtml","browse"), "html", true, false), Map(("query", initial)), true)
    }
  
    // requests that are not caught and handled here are passed on to the snippets/templates 
    LiftRules.statelessDispatchTable.append(Rest.handler)
    
    // Use HTML5 for rendering
    //LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))
    
    // disable auto-include of lift-javascript for ajax
    LiftRules.autoIncludeAjax = _ => false
    //LiftRules.ajaxPath = ":ajax_request"
    //LiftRules.resourceServerPath = "script"
    // disable auto-include of lift-javascript for comet actors
    LiftRules.autoIncludeComet = _ => false
    // disable GC to avoid javascript error
    LiftRules.enableLiftGC = false 
    
    // run the server
    Manager.start()
   }
}

