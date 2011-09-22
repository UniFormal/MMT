package info.kwarc.mmt.web

import info.kwarc.mmt._
import web._
import api._
import frontend._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common.{Box,Empty,Full}

import java.net.HttpURLConnection;
import java.net._
import java.io._
import info.kwarc.mmt.mizar.test.MwsService
import info.kwarc.mmt.tptp._
import scala.xml._

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
          // for now: assume always incoming edges are desired
          val mmtpath = Path.parse(query, Manager.basepath)
          val node = Get.incoming(mmtpath)
          XmlResponse(node)
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
          val resp = Manager.controller.backend.mws match {
            case None => <error message="no MathWebSearch backend defined"/>
            case Some(mws) =>
              r.xml.toOption match {
                
                // TPTP
                case None =>
                  r.body.toOption match {
                    case None => <error message="empty query string"/>
                    case Some(body) =>
                      try {
                        val b = new String(body)
                        query match {
                          case "tptp" =>
                            val Offset = r.header("Offset") match {
                              case Full(s) => try s.toInt catch {case _ => 0}
                              case _ => 0
                            }
                            val Size = r.header("Size") match {
                              case Full(s) => try s.toInt catch {case _ => 30}
                              case _ => 30
                            }
                            val translator = new TptpTranslator()
                            val translated = translator.translateFormula(b)
                            translated match {
                              case Some(x) =>
                                val node = TptpTranslator.toMwsQuery(x.toCML, Offset, Size)
//                                val res = node.map(utils.xml.post(new URL(mws.toJava.toURL), _))
                                val res = node.map(utils.xml.post(new URL("http://localhost:20000"), _))
                                val total = res.foldRight(0)((r,x) => x + (r \ "@total").text.toInt)
                                val size = res.foldRight(0)((r,x) => x + (r \ "@size").text.toInt)
                                val answrs = res.flatMap(_.child)
                                <mws:answset total={total.toString} size={size.toString} xmlns:mws="http://www.mathweb.org/mws/ns">{answrs}</mws:answset>
                              case None => <error><message>Error translating the given query to OMDoc</message></error>
                            }
                          case _ => <error><message>Only TPTP queries can be specified as string</message></error>
                        }
                      } catch {
                        case e: ParseError => <error><message>{e.getMessage}</message></error>
                        case _ => <error><message>Translation Failed</message></error>
                      }
                  }
                  
                // Mizar
                case Some(body) => try {
                    val input = query match {
                      case "mizar" => 
                        val mmlVersion = r.header("MMLVersion") match {
                          case Full(s) => s
                          case _ => "4.166" 
                        }
                        val currentAid = r.header("Aid") match {
                          case Full(s) => s
                          case _ => "HIDDEN"
                        }
                        val Offset = r.header("Offset") match {
                          case Full(s) => try s.toInt catch {case _ => 0}
                          case _ => 0
                        }
                        val Size = r.header("Size") match {
                          case Full(s) => try s.toInt catch {case _ => 30}
                          case _ => 30
                        }
                        val q = MwsService.parseQuery(body, currentAid, mmlVersion,Offset,Size) // translate mizar-xml query
                        val qlist = MwsService.applyImplicitInferences(q)
                        qlist
                      case _ => List(body) // assume content math query by default
                    }
                    val res = input.map(utils.xml.post(mws.toJava.toURL, _))
                    val total = res.foldRight(0)((r,x) => x + (r \ "@total").text.toInt)
                    val size = res.foldRight(0)((r,x) => x + (r \ "@size").text.toInt)
                          
                    val answrs = res.flatMap(_.child)
                    <mws:answset total={total.toString} size={size.toString} xmlns:mws="http://www.mathweb.org/mws/ns">{answrs}</mws:answset>  
                  } catch {
                    case e: ParseError => <error><message>{e.getMessage}</message><input>{body}</input></error>
                    case _ => <error><message>Invalid MizXML, Translation Failed</message></error>
                  }
              }
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