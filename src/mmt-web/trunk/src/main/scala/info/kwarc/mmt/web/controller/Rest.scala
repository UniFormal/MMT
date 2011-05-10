package info.kwarc.mmt.web.controller

import info.kwarc.mmt.web.controller._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common.{Box,Empty,Full}


object Rest extends RestHelper {
   serve {
       case r : Req if List("xml","text") contains r.param("format").getOrElse("") =>
          val doc = r.param("document").getOrElse("")
          val mod = r.param("module").getOrElse("")
          val sym = r.param("symbol").getOrElse("")
          val act = r.param("action").getOrElse("")
          try {
             val node = Manager.doGet(doc, mod, sym, act)
           r.param("format").get match {
              case "xml" => () => Full(XmlResponse(node))
              case "text" => () => Full(PlainTextResponse(node.toString))
                case f => throw ParseError("illegal format: " + f)
           }
          } catch {
             case e => () => Full(PlainTextResponse("get error\n\n" + e.getMessage))
          }
       case r : Req if r.path.partPath.headOption == Some(":pathtree") =>
          val q = ReqHelpers.query(r.request)
          val node = PathTree(q)
          val ct = "Content-Type" -> "text/html; charset=utf-8"
          () => Full(XmlResponse(node))
       case r: Req if r.path.partPath.headOption == Some(":uom") =>
          val q = ReqHelpers.query(r.request)
          q match {
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
   }
}