package info.kwarc.mmt.web

import info.kwarc.mmt._
import web._
import api._
import frontend._
import backend._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common.{Box,Empty,Full}

import java.net.HttpURLConnection
import java.net._
import java.io._
import scala.xml._

case class ServerError(n: Node) extends java.lang.Throwable

object Rest {
  def div(n: List[Node]) : Node = Elem(null, "div", Null, NamespaceBinding(null, utils.xml.namespace("xhtml"), TopScope), n :_*)
  def div(s: String) : Node = div(List(scala.xml.Text(s)))
  val services = List(":tree", ":query", ":graph", ":search", ":uom", ":mmt", ":breadcrumbs", ":admin")
  def applicable(p: ParsePath) = services contains p.partPath.headOption.getOrElse("")
  val handler : LiftRules.DispatchPF = {case r : Req if applicable(r.path) =>
      val path : List[String] = r.path.wholePath
      val query : String = ReqHelpers.query(r.request) 
      val resp : LiftResponse = path match {
        case ":tree" :: _ =>
          val node = Get.tree(query)
          //val ct = "Content-Type" -> "text/html; charset=utf-8"
          XmlResponse(node)
        case ":query" :: _ =>
           // default incoming edges
           if (query == "") {
              val mmtpath = Path.parse(query, Manager.basepath)
              val node = Get.incoming(mmtpath)
              XmlResponse(node)
           } else {
              case class Error(n: Node) extends java.lang.Throwable
              try {
                 val bodyWS = r.xml.toOption.getOrElse(throw Error(<error message="no body found (did you use Content-Type=text/xml?)"/>))
                 val body = scala.xml.Utility.trim(bodyWS)
                 val q = ontology.Query.parse(body)
                 val res = try {Manager.eval.evaluate(q)}
                           catch {
                              case ParseError(s) => throw Error(<error message={s}/>)
                              case GetError(s) => throw Error(<error message={s}/>)
                           }
                 val resp = res.toNode
                 XmlResponse(resp)
              }
              catch {case Error(n) => XmlResponse(n)}
           }
        case ":graph" :: _ =>
          val mmtpath = Path.parse(query, Manager.basepath)
          val json = Get.graph(mmtpath)
          JsonResponse(json)
        case ":breadcrumbs" :: _ =>
          val mmtpath = Path.parse(query, Manager.basepath)
          val node = scala.xml.Utility.trim(Get.breadcrumbs(mmtpath))
          XmlResponse(node)
        case ":admin" :: _ =>
          try {
            Manager.report.record
            val c = query.replace("%20", " ")
            val act = frontend.Action.parseAct(c, Manager.basepath, Manager.controller.getHome)
            Manager.controller.handle(act)
            val r = Manager.report.recall
            Manager.report.clear
            XmlResponse(div(r reverseMap div))
          } catch {
            case e: api.Error =>
              Manager.report(e)
              Manager.report.clear
              XmlResponse(div("error: " + e.msg))
          }
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
        case ":search" :: _ =>
      val controller = Manager.controller
      val resp = try {
         val mws = controller.backend.mws.getOrElse(throw ServerError(<error message="no MathWebSearch backend defined"/>))
         val body = r.body.getOrElse(throw ServerError(<error message="no query given"/>))
         val offset = r.header("Offset").toOption match {
            case Some(s) => try s.toInt catch {case _ => 0}
            case _ => 0
         }
         val size = r.header("Size").toOption match {
            case Some(s) => try s.toInt catch {case _ => 30}
            case _ => 30
         }
         val mwsqs : List[Node] = query match {
             case "mizar" =>
                val bodyXML = scala.xml.XML.loadString(body.mkString).head
                val mmlVersion = r.header("MMLVersion").toOption match {
                   case Some(s) => s
                   case _ => "4.166" 
                }
                val currentAid = r.header("Aid").toOption match {
                   case Some(s) => s
                   case _ => "HIDDEN"
                }
                val mizar = java.lang.Class.forName("info.kwarc.mmt.mizar.test.MwsService").asInstanceOf[java.lang.Class[QueryTransformer]].newInstance
                mizar.transformSearchQuery(bodyXML, List(currentAid, mmlVersion))
             case "tptp" =>
                 val bodyString = body.mkString
                 val tptp = java.lang.Class.forName("info.kwarc.mmt.tptp.TptpCompiler").asInstanceOf[java.lang.Class[QueryTransformer]].newInstance
                 tptp.transformSearchQuery(scala.xml.Text(bodyString), Nil)
             case _ => List(scala.xml.XML.loadString(body.mkString).head) // default: body is forwarded to MWS untouched
          }
          def wrapMWS(n: Node) : Node = <mws:query output="xml" limitmin={offset.toString} answsize={size.toString}>{n}</mws:query>
          val res = mwsqs.map(q => utils.xml.post(mws.toJava.toURL, wrapMWS(q)))
          val total = res.foldRight(0)((r,x) => x + (r \ "@total").text.toInt)
          val totalsize = res.foldRight(0)((r,x) => x + (r \ "@size").text.toInt)
          val answrs = res.flatMap(_.child)
          <mws:answset total={total.toString} size={totalsize.toString} xmlns:mws="http://www.mathweb.org/mws/ns">{answrs}</mws:answset>
      } catch {
         case ServerError(n) => n
         case e: ParseError => <error><message>{e.getMessage}</message></error>
         case _ => <error><message>error translating query</message></error>
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
          val textresponse : Boolean = try {
            Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, Manager.basepath, Manager.controller.getHome) match {
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

object ReqHelpers {
  // extracts the URI query from a request
  def query(r : net.liftweb.http.provider.HTTPRequest) = r.queryString.getOrElse("") //TODO query has to be decoded
  // extracts the URI path from a request
  def path(r : net.liftweb.http.provider.HTTPRequest) = new java.net.URI(r.uri).getPath()
  // extracts the content Type from a request
  def ctype(r : net.liftweb.http.provider.HTTPRequest) = r.header("Accept").getOrElse("")
}