package info.kwarc.mmt.web.controller

import info.kwarc.mmt.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common.{Box,Empty,Full}


object Rest {
   def applicable(p: ParsePath) = List(":tree", ":query", ":uom", ":mmt", ":breadcrumbs") contains p.partPath.headOption.getOrElse("")
   val handler : LiftRules.DispatchPF = {case r : Req if applicable(r.path) =>
      val path : List[String] = r.path.wholePath
      val query : String = ReqHelpers.query(r.request) 
      val resp : LiftResponse = path match {
          case ":tree" :: _ =>
             val node = snippet.Get.tree(query)
             //val ct = "Content-Type" -> "text/html; charset=utf-8"
             XmlResponse(node)
          case ":query" :: _ =>
             // for now: assume always incoming edges are desired
             val mmtpath = Path.parse(query, Manager.basepath)
             val node = snippet.Get.incoming(mmtpath)
             //val ct = "Content-Type" -> "text/html; charset=utf-8"
             XmlResponse(node)
          case ":breadcrumbs" :: _ =>
             val mmtpath = Path.parse(query, Manager.basepath)
             val node = scala.xml.Utility.trim(snippet.Get.breadcrumbs(mmtpath))
             XmlResponse(node)
          case ":uom" :: _ =>
             val resp = query match {
                case "register" => <error message="not implemented yet"/>
                case "simplify" =>
                    r.xml.toOption match {
                       case None => <error message="no body found (did you use Content-Type=text/xml?)"/>
                       case Some(body) => 
   //               val bodyNode = javax.xml.parsers.DocumentBuilderFactory
   //                .newInstance()
   //                .newDocumentBuilder()
   //                //.parse(new java.io.StringInputStream(body)
   //                .parse(new org.xml.sax.InputSource(new java.io.StringReader(body)))
   //                .getDocumentElement()
                        try {
                           val input = objects.Obj.parseTerm(r.xml.get, Manager.basepath)
                           val output = Manager.uom.simplify(input)
                           output.toNode
                        } catch {
                           case e: ParseError => <error><message>{e.getMessage}</message><input>{body}</input></error>
                        }
                    }
                case _ => <error message="illegal command"/>
             }
             XmlResponse(resp)
          case ":mmt" :: _ =>
              val comps = query.split("\\?",-1)
              val (doc, mod, sym, act) = comps.length match {
                 case 1 => (comps(0), "", "", "")
                 case 2 => (comps(0), comps(1), "", "")
                 case 3 => (comps(0), comps(1), comps(2), "")
                 case _ =>
                    val rest = comps.drop(3).mkString("","?","")
                    (comps(0), comps(1), comps(2), rest.replace("_", " "))
              }
              // "xml": render and xml response; "text": render and text response; "xhtml": render via template/snippet and xml response
              val textresponse : Boolean = try {
                  Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, Manager.basepath) match {
                  case DefaultGet(p) => p match {
                     case Present(_,_) => ReqHelpers.ctype(r.request) match {
                        case "text/xml" => false
                           case _ => false
                        }
                     case ToNode(_) | Deps(_) => false
                     case ToString(_) => true
                  }
                      case _ => false
                  }
               } catch {
                  case _ => false
               }
               try {
                  val node = Manager.doGet(doc, mod, sym, act)
                  if (textresponse)
                     PlainTextResponse(node.toString)
                  else
                     XmlResponse(node)
               } catch {
                 case e => XmlResponse(<div>{"get error:\n\n" + e.getMessage}</div>)
               }
          case p => XmlResponse(<error path={p.toString}/>)  // impossible
      }
      () => Full(resp)
   }
}