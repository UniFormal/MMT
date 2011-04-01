package bootstrap.liftweb

import info.kwarc.mmt.web.controller._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api._
import net.liftweb._
import net.liftweb.http._
import net.liftweb.common.{Box,Empty,Full}

/**
  * A class that's instantiated early and run. 
  * It allows the application to modify lift's environment.
  */
class Boot {
   // extracts the URI query from a request
   private def query(r : net.liftweb.http.provider.HTTPRequest) = r.queryString.getOrElse("")
   // extracts the URI path from a request
   private def path(r : net.liftweb.http.provider.HTTPRequest) = new java.net.URI(r.uri).getPath()
   // extracts the content Type from a request
   private def ctype(r : net.liftweb.http.provider.HTTPRequest) = r.header("Accept").getOrElse("")
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
         Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, Controller.basepath) match {
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
      RewriteResponse(Req.NilPath, params, true)
   }
   
   /** init method */
   def boot {
     // where to search snippets
     LiftRules.addToPackages("ombase")

     // add URL rewrites
     LiftRules.statelessRewrite.append {
          //admin interface
          case RewriteRequest(ParsePath(List(":admin"), _, _, _), GetRequest, request) =>
             val command = query(request)
             RewriteResponse(ParsePath(List("admin"), "html", true, false), 
                             Map(("command", command), ("format", "xhtml")),
                             true)
          //auxiliary files: css and javascript
          case RewriteRequest(ppath @ ParsePath(hd :: _, _, _, _), GetRequest, request)
             if hd == "css" || hd == "script" || hd == ":pathtree" =>
                RewriteResponse(ppath, Map.empty, true)
         /* URI of the form CATALOG-AUTHORITY/;?doc?mod?sym?params
          * Ideally: URI of the form CATALOG-AUTHORITY?doc?mod?sym?params
          * But firefox transforms "path" to "/path" if path(0) != "/"
		  * so the next best thing to an empty path is the path "/;"
		  */
          case RewriteRequest(ParsePath(List(";"), _, _, _), GetRequest, request) => 
      		 rewr(query(request), ctype(request))
         /* URI of the form OMBASE-AUTHORITY/path?mod?sym?params where doc := OMBASE-AUTHORITY/path
          * lift transforms "" and "/" to "/index" (see net.liftweb.http.Req.parsePath)
		  * lift also removes empty path segments
          * path(request) yields the original (client-sent) path
          */
          case RewriteRequest(_, GetRequest, request) =>
               rewr(path(request) + "?" + query(request), ctype(request))
    }
  
    // requests that are not caught and handled here are passed on to the snippets/templates 
    LiftRules.statelessDispatchTable.append {
       case r : Req if List("xml","text") contains r.param("format").getOrElse("") =>
          val doc = r.param("document").getOrElse("")
          val mod = r.param("module").getOrElse("")
          val sym = r.param("symbol").getOrElse("")
          val act = r.param("action").getOrElse("")
          try {
             val node = Controller.doGet(doc, mod, sym, act)
	         r.param("format").get match {
	            case "xml" => () => Full(XmlResponse(node))
	            case "text" => () => Full(PlainTextResponse(node.toString))
                case f => throw ParseError("illegal format: " + f)
	         }
          } catch {
             case e => () => Full(PlainTextResponse("get error\n\n" + e.getMessage))
          }
       case r : Req if r.path.partPath.headOption == Some(":pathtree") =>
          val q = query(r.request)
          val node = PathTree(q)
          val ct = "Content-Type" -> "text/html; charset=utf-8"
          () => Full(XmlResponse(node))
    }
    
    // disable auto-include of lift-javascript for ajax
    LiftRules.autoIncludeAjax = _ => false
    //LiftRules.ajaxPath = ":ajax_request"
    //LiftRules.resourceServerPath = "script"
    // disable auto-include of lift-javascript for comet actors
    LiftRules.autoIncludeComet = _ => false
    // disable GC to avoid javascript error
    LiftRules.enableLiftGC = false 
    
    // run the server
    Controller.start()
   }
}

