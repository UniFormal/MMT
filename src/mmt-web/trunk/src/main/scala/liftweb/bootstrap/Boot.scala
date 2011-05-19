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
   /**
    * @param action MMTURI?PP where PP is the action (possibly with _ for space)
    * @param ctype the accepted content type
    * @return the rewritten request
    */
   private def rewr(action : String, ctype : String) = {
      val comps = action.split("\\?",-1)
      val (doc, mod, sym, act) = comps.length match {
         case 1 => (comps(0), "", "", "")
         case 2 => (comps(0), comps(1), "", "")
         case 3 => (comps(0), comps(1), comps(2), "")
         case _ =>
           var rest = comps.drop(3).mkString("","?","")
           (comps(0), comps(1), comps(2), rest.replace("_", " "))
      }
      // "xml": render and xml response; "text": render and text response; "xhtml": render via template/snippet and xml response
      val format = try { 
         Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, Manager.basepath) match {
           case DefaultGet(p) => p match {
              case Present(_,_) => ctype match {
                   case "text/xml" => "xml"
                   case _ => "xhtml"
                }
              case ToNode(_) | Deps(_) => "xml"
              case ToString(_) => "text"
           }
             case _ => "xml"
         }
      } catch {
         case _ => "xml" 
      }
      val params = Map( ("document", doc), ("module", mod), ("symbol", sym), ("action", act), ("format", format) )
      RewriteResponse(Req.NilPath, params, false)
   }
   
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

