package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.vollki.FullsTeXGraph
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.{ErrorReturn, SHTML, STeXServer}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLNodeWrapper, HTMLParser, HTMLRule, SHTMLNode, SHTMLRule, SHTMLState}
//import info.kwarc.mmt.stex.xhtml.{HTMLArg, HTMLComp, HTMLLanguageComponent, HTMLNode, HTMLParser, HasHead, NotationComponent}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}

import scala.xml.parsing.XhtmlParser
import scala.xml.{Elem, Node}

trait SHTMLDocumentServer { this : STeXServer =>
  protected def documentRequest(request: ServerRequest) : ServerResponse = request.path.lastOption match {
    case Some("document") =>
      request.query match {
        case "" =>
          ServerResponse("Empty Document path", "txt")
        case s =>
          val ret = doDocument(s)
          val bd = ret.get("div")()("body").head
          bd.plain.attributes.remove((bd.namespace, "style"))
          bd.plain.attributes.remove((bd.namespace, "id"))
          ServerResponse(ret.toString, "text/html")
      }
    case Some("documentTop") =>
      request.query match {
        case "" =>
          ServerResponse("Empty Document path", "txt")
        case s =>
          ServerResponse(doDocument(s).toString, "text/html")
      }
    case Some("fragment") =>
      request.query match {
        case "" =>
          ServerResponse("Empty fragment", "txt")
        case ps =>
          val (comp, lang) = ps.split('&') match {
            case Array(a) => (a, None)
            case Array(a, l) if l.startsWith("language=") => (a, if (l.drop(9).isEmpty) None else Some(l.drop(9)))
            case _ => (ps, None)
          }
          val path = Path.parse(comp)
          doFragment(path, lang).getOrElse(
            ServerResponse("Missing fragment","txt")
          )
      }
    case Some("symbol") =>
      request.query match {
        case "" =>
          ServerResponse("Empty Document path", "txt")
        case s =>
          var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
          html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/declaration?" + s)
          html = html.replace("BASE_URL_PLACEHOLDER", "")
          ServerResponse(html, "text/html")
      }
    case Some("declaration") =>
      request.query match {
        case "" =>
          ServerResponse("Empty fragment", "txt")
        case ps =>
          val (comp, lang) = ps.split('&') match {
            case Array(a) => (a, None)
            case Array(a, l) if l.startsWith("language=") => (a, if (l.drop(9).isEmpty) None else Some(l.drop(9)))
            case _ => (ps, None)
          }
          val path = Path.parse(comp)
          doDeclaration(path, lang)
      }
    case Some("variable") =>
      ServerResponse("<b>TODO</b>","text/html")
    case Some("fulldocument") =>
      request.query match {
        case "" =>
          ServerResponse("Empty Document path", "txt")
        case s =>
          var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
          html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/documentTop?" + s)
          html = html.replace("BASE_URL_PLACEHOLDER", "")
          html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
          ServerResponse(html, "text/html")
      }
    case _ =>
      throw ErrorReturn("Unknown path: " + request.path.mkString)
  }

  private case class PresentationRule(key: String, newobj: (String, HTMLParser.ParsingState, HTMLNode) => Option[SHTMLNode], override val priority: Int = 0) extends SHTMLRule(priority) {
    def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
      attrs.find(_._1 == key).flatMap { p =>
        try {
          newobj(p._2, s, n)
        } catch {
          case _ => None
        }
      }
    }
  }

  def doDocument(uri: String) = {
    val filecontent = getDocument(uri)
    val body = filecontent.get("body")()().head
    body
  }

  def getDocument(uri: String): HTMLNode = {
    uri match {
      case s if s.startsWith("group=") =>
        val grp = s.drop(6)
        val doc = this.emptydoc
        doc._2.add(<b style="text-align: center">
          {"Group: " + grp}
        </b>)
        doc._1
      case s if s.startsWith("archive=") =>
        val (id, path) = {
          s.drop(8).split('&') match {
            case Array(s) => (s, FilePath(Nil))
            case Array(s, r) if r.startsWith("filepath=") =>
              (s, FilePath(r.drop(9).split('/').toList))
            case _ =>
              print("")
              ???
          }
        }
        if (path.isEmpty) {
          val (doc, bd) = this.emptydoc
          controller.backend.getArchive(id) match {
            case None =>
              bd.add(<b>
                {"No archive with id " + id + " found!"}
              </b>)
            case Some(a) =>
              bd.add(<b style="text-align: center">
                {a.id}
              </b>)
              a.properties.collectFirst {
                case ("description", d) if (a.root / "META-INF" / d).exists() =>
                  bd.add(File.read(a.root / "META-INF" / d))
                case ("description", d) if (a.root / "meta-inf" / d).exists() =>
                  bd.add(File.read(a.root / "meta-inf" / d))
                case ("teaser", t) =>
                  bd.add("<div>" + t + "</div>")
              }
          }
          doc
        } else {
          controller.backend.getArchive(id) match {
            case None =>
              val (doc, bd) = this.emptydoc
              bd.add(<b>
                {"No archive with id " + id + " found!"}
              </b>)
              doc
            case Some(a) =>
              val top = a / RedirectableDimension("xhtml")
              val fn = top / path
              val state = new ParsingState(controller, presentationRules)
              if (fn.exists() && fn.isDirectory) {
                if ((fn / "index.xhtml").exists()) {
                  val ret = HTMLParser(fn / "index.xhtml")(state)
                  ret
                } else {
                  val (doc, bd) = this.emptydoc
                  bd.add(<b>
                    {id + "/" + path}
                  </b>)
                  doc
                }
              } else if (fn.exists()) {
                val ret = HTMLParser(fn)(state)
                ret
              }
              else {
                val (doc, bd) = this.emptydoc
                bd.add(<b>
                  {"Document does not exist: " + id + "/" + path}
                </b>)
                doc
              }
          }
        }
      case _ =>
        ???
    }
  }

  def getLanguage(elem: HTMLNode) = {
    val top = getTop(elem)
    ""
    /*
    top.get()((HTMLParser.ns_html, "property", "shtml:language"))().headOption match {
      case Some(l: HTMLLanguageComponent) => l.resource
      case _ => ""
    }
     */
  }

  private def getTop(elem: HTMLNode): HTMLNode = if (elem.label == "body") elem else elem.plain.parent match {
    case Some(p) => getTop(p)
    case _ => elem
  }
/*
  def sidebar(elem: HTMLNode, content: List[Node]) = {
    def parent(e: HTMLNode): Option[(HTMLNode, HTMLNode)] = e.plain.parent match {
      case Some(p) if !p.isMath => Some((p, e))
      case Some(p) => parent(p)
      case None => None
    } //if (e.isMath) parent(e.parent.get) else e

    parent(elem).foreach { case (p, c) =>
      p.addBefore(<div class="sidebar">
        {content}
      </div>, c)
    }
    //val sidenotes = getTop(elem).get()()("sidenote-container").headOption
    //sidenotes.foreach(_.add(<div>{content}</div>))
  }


  def makeButton(urlshort: String, urllong: String, elem: Node, withclass: Boolean = true): Node = // makesafe(XHTML(
    <span class={if (withclass) "propbtn" else ""} style="display:inline" data-overlay-link-click={urllong} data-overlay-link-hover={urlshort}>
      {elem}
    </span>
  //))

  def makePostButton(elem: Node, target: String, data: (String, String)*): Node = // makesafe(XHTML(
    <form method="post" action={target} class="inline" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner" style="display:none">
      {data.map { p => <input type="hidden" name={p._1} value={p._2} class="inline"/> }}<span onclick="this.closest('form').submit();" type="submit" class="propbtn" style="display:inline">
      {elem}
    </span>
    </form>
  //))

  def makesafe(xh: HTMLNode) = {
    new HTMLNode(xh.state, xh.namespace, xh.label) {
      override def node = xh.node

      override def toString: String = xh.toString
    }
  }
*/


  private def hasAttribute(node:HTMLNode,s:String) = node.plain.attributes.contains((HTMLParser.ns_shtml,s))
  private def getAttribute(node: HTMLNode, s: String) = node.plain.attributes.get((HTMLParser.ns_shtml, s))
  def overlay(elem: HTMLNode/*, urlshort: String, urllong: String*/): Unit = {
    if (elem.plain.classes.contains("overlay-parent")) return
    val id = elem.hashCode().toString
    elem.plain.attributes((elem.namespace, "id")) = id
    elem.plain.classes ::= "hasoverlay-parent"

    //elem.classes ::= "hasoverlay" //:: elem.classes
    def pickelems(n: HTMLNode, inarg: Boolean = false): List[HTMLNode] = n match {
      case comp if hasAttribute(comp, "comp") || hasAttribute(comp, "varcomp") || hasAttribute(comp, "definiendum") =>
        List(comp)
      //case a if hasAttribute(a,"arg") && hasAttribute(a,"term") => Nil
      case a if hasAttribute(a,"arg") => Nil //a.children.flatMap(pickelems(_, true))
      //case t if hasAttribute(t,"term") && inarg => Nil
      case o => o.children.flatMap(pickelems(_, inarg))
    }

    val targets = pickelems(elem)
    targets.foreach { e =>
      e.plain.classes ::= "group-highlight"
      e.plain.attributes((e.namespace, "data-highlight-parent")) = id
      if (hasAttribute(e,"comp")) { getAttribute(e,"comp").foreach{v =>
        val lang = getLanguage(e)
        e.plain.classes ::= "symcomp"
        val urlshort = "/:" + this.pathPrefix + "/fragment?" + v + "&language=" + lang
        val urllong = "/:" + this.pathPrefix + "/declaration?" + v + "&language=" + lang
        e.plain.attributes((elem.namespace, "data-overlay-link-hover")) = urlshort
        e.plain.attributes((elem.namespace, "data-overlay-link-click")) = urllong
      }} else if (hasAttribute(e,"varcomp")) {
        e.plain.classes ::= "varcomp" // TODO
      } else if (hasAttribute(e,"definiendum")) {
        e.plain.classes ::= "definiendum" // TODO
      }
    }
  }

  def doDeclaration(path: Path, language: Option[String]): ServerResponse = {
    controller.getO(path) match {
      case Some(c: Constant) =>
        val (_, body) = this.emptydoc
        body.add(doDeclHeader(c))
        getFragment(path, language) match {
          case Some(htm) =>
            body.add(<hr/>)
            body.add(htm)
          case _ =>
        }
        ServerResponse("<body>" + body.toString + "</body>", "text/html")
      case Some(d) =>
        ServerResponse("Not yet implemented: " + d.getClass.toString, "txt")
      case _ =>
        ServerResponse("Declaration not found", "txt")
    }
  }

  def doDeclHeader(c: Constant): Elem = {
    <div>
      <table>
        <tr>
          <td>
            <font size="+2">
              {" ☞ "}
            </font> <code>
            {c.path.toString}
          </code>
          </td> <td>
          {if (controller.extman.get(classOf[FullsTeXGraph.type]).contains(FullsTeXGraph)) {
            <a href={"/:vollki?path=" + c.parent.toString} target="_blank" style="pointer-events:all;color:blue">
              {"> Guided Tour"}
            </a>
          } else <span></span>}
        </td>
        </tr>
      </table> <hr/>
      <table>
        {this.getMacroName(c) match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(s) => <tr>
          <td style="padding:3px">
            <b>TeX Macro:</b>
          </td> <td>
            <code>
              {"\\" + s}
            </code>
          </td>
        </tr>
      }}{def td[A](a: A) = <td style="border:1px solid;padding:3px">
        {a}
      </td>

      this.getNotations(c) match {
        case Nil => <tr>
          <td></td> <td></td>
        </tr>
        case ls =>
          <tr>
            <td style="padding-right:3px">
              <b>Notations:</b>
            </td> <td>
            <table>
              <tr>
                {td("identifier")}{td("notation")}{td("operator notation")}{td("in module")}
              </tr>{ls.map(n =>
              <tr>
                {td(n.id match {
                case "" => "(None)"
                case s => s
              })}{td(<math xmlns="http://www.w3.org/1998/Math/MathML">
                {this.htmlpres.doNotation(n.notation.plain.node)}
              </math>)}{td(n.op match {
                case Some(n) => <math xmlns="http://www.w3.org/1998/Math/MathML">
                  {this.htmlpres.doNotation(n.plain.node)}
                </math>
                case None => <span></span>
              })}{td(if (n.in.path != c.parent) n.in.path.toString else "(here)")}
              </tr>
            )}
            </table>
          </td>
          </tr>
      }}{c.tp match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(tp) =>
          <tr>
            <td style="padding:3px">
              <b>Type:</b>
            </td> <td>
            {this.xhtmlPresenter.asXML(tp, Some(c.path $ TypeComponent))}
          </td>
          </tr>
      }}{c.df match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(df) =>
          <tr>
            <td style="padding:3px">
              <b>Definiens:</b>
            </td> <td>
            {this.xhtmlPresenter.asXML(df, Some(c.path $ DefComponent))}
          </td>
          </tr>
      }}
      </table>
    </div>
  }

  def doFragment(path: Path, language: Option[String]) = {
    getFragment(path, language) match {
      case Some(htm) =>
        val (doc, body) = this.emptydoc
        body.plain.attributes((HTMLParser.ns_html, "style")) = "background-color:white"
        stripMargins(doc)
        val border = body.add(<div style="font-size:small"/>)
        //border.add(<font size="+2">{" ☞ "}</font>)
        //border.add(<code>{path.toString}</code>)
        //border.add(<hr/>)
        border.add(htm)
        val nbody = doc.get("body")()().head
        Some(ServerResponse(nbody.toString, "text/html"))
      case None =>
        Some(ServerResponse("Empty fragment", "txt"))
    }
  }

  def getFragment(path: Path, language: Option[String]) = {
    path match {
      case mp: MPath =>
        controller.simplifier(mp)
      case gn: GlobalName =>
        controller.simplifier(gn.module)
    }
    controller.getO(path) match {
      case Some(elem) =>
        elem match {
          case c: Constant =>
            val state = new ParsingState(controller, presentationRules)
            Some(HTMLParser.apply(getFragmentDefault(c, language))(state))
          case _ =>
            ???
        }
      case _ => None
    }
  }

  def getFragmentDefault(c : Constant,language:Option[String]) : String = {
    this.getSymdocs(c.path) match { // TODO language
      case a :: _ => a.toString()
      case _ =>
        val res = "Symbol <b>" + c.name + "</b> in module " + (SourceRef.get(c) match {
          case Some(sr) =>
            controller.backend.resolveLogical(sr.container) match {
              case Some((a, f)) =>
                val url = "/:sTeX/browser/fulldocument" +
                  "?archive=" + a.id + "&filepath=" + (f.init ::: f.last.replace(".tex", ".xhtml") :: Nil).mkString("/")
                s"<a href='${url}'>${c.parent.name.toString}</a>"
              case _ => c.parent.name.toString
            }
          case _ => c.parent.name.toString
        })
        "<div>" + res + "</div>"
    }
  }

  def stripMargins(ltx: HTMLNode) = {
    val body = ltx.get("body")()().head
    body.plain.attributes((HTMLParser.ns_html, "style")) = "margin:0;padding:0;"
    val doc = body.get("div")()("body").head
    doc.plain.attributes((HTMLParser.ns_html, "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
  }

  lazy val presentationRules: List[HTMLRule] = List(
    PresentationRule("term", (_, _, node) => {
      overlay(node);
      Some(new SHTMLNode(node) {
        override def onAdd: Unit = overlay(this)

        override def copy: HTMLNode = node.copy
      })
    }),
    PresentationRule("definiendum", (_, _, node) => {
      overlay(node);
      Some(new SHTMLNode(node) {
        override def onAdd: Unit = overlay(this)

        override def copy: HTMLNode = node.copy
      })
    }),
    PresentationRule("inputref", (v, _, node) => {
      val dp = Path.parseD((if (v.endsWith(".tex")) {
        v.dropRight(4)
      } else v) + ".omdoc", NamespaceMap.empty)
      controller.getO(dp) match {
        case Some(d: Document) =>
          controller.backend.resolveLogical(d.path.uri) match {
            case Some((a, ls)) =>
              val path = ls.init.mkString("/") + "/" + ls.last.dropRight(5) + "xhtml"
              node.plain.attributes((node.namespace, "style")) = ""
              node.plain.attributes.remove((HTMLParser.ns_shtml, "visible"))
              node.plain.classes = List("inputref")
              node.plain.attributes((node.namespace, "data-inputref-url")) = "/:" + pathPrefix + "/document?archive=" + a.id + "&filepath=" + path
              this.getTitle(d).foreach(n => node.add(n))
            case _ =>
          }
        case _ =>
      }
      None
    })
  )

  lazy val presentationRulesOld : List[PartialFunction[HTMLNode,Unit]] = List(/*
      {case spf : HTMLProofFrame =>
        spf.children.collectFirst {case t:HTMLSProoftitle => t} match {
          case Some(ttl) =>
            spf.children.collectFirst {case t : HTMLSProofbody => t} match {
              case Some(bd) =>
                ttl.attributes((ttl.namespace,"data-collapse-title")) = "true"
                bd.attributes((bd.namespace,"data-collapse-body")) = "true"
                spf.attributes((spf.namespace,"data-collapsible")) = if (spf.expanded) "true" else "false"
            }
          case _ =>
        }
      },
      {case iref : HTMLInputref =>
        val dp = Path.parseD(iref.resource + ".omdoc",NamespaceMap.empty)
        controller.getO(dp) match {
          case Some(d:Document) =>
            controller.backend.resolveLogical(d.path.uri) match {
              case Some((a,ls)) =>
                val path = ls.init.mkString("/") + "/" + ls.last.dropRight(5) + "xhtml"
                val ipref = <div class="inputref" data-inputref-url={"/:" + server.pathPrefix + "/document?archive=" + a.id + "&filepath=" + path}>
                  {d.metadata.get(STeX.meta_doctitle).headOption.map(_.value match {
                    case OMFOREIGN(node) => node
                    case _ => ""
                  }).getOrElse("")}
                </div>
                iref.parent.foreach(_.addAfter(
                  if (iref.ancestors.exists(_.isInstanceOf[HTMLFrame])) {
                    <div class="inputref-container">
                      {ipref}
                    </div>
                  } else ipref
                  ,iref))
              case _ =>
            }
          case _ =>
        }
      },
      {case thm: HTMLTheory =>
        sidebar(thm, <b style="font-size: larger">Theory: {thm.name.toString}</b> :: Nil)
      },
      {case s: HTMLSymbol =>
        controller.getO(s.path) match {
          case Some(c : Constant) =>
            sidebar(s,{<span style="display:inline">Constant {makeButton(
              "/:" + server.pathPrefix + "/fragment?" + c.path + "&language=" + getLanguage(s),
              "/:" + server.pathPrefix + "/declaration?" + c.path + "&language=" + getLanguage(s)
              ,scala.xml.Text(c.name.toString)
            )}<code>(\{s.macroname})</code></span>} :: Nil)
          case _ =>
        }
      },
      {
        case t : HTMLTopLevelTerm if t.orig.isInstanceOf[HTMLDefiniendum] =>
          overlay(t, ""/*/:" + server.pathPrefix + "/declheader?" + t.orig.asInstanceOf[HTMLDefiniendum].head.toString*/,
            "/:" + server.pathPrefix + "/declaration?" + t.orig.asInstanceOf[HTMLDefiniendum].head.toString  + "&language=" + getLanguage(t))
      },
      {
        case t : HTMLDefiniendum =>
          overlay(t, ""/*"/:" + server.pathPrefix + "/declheader?" + t.head.toString*/,
            "/:" + server.pathPrefix + "/declaration?" + t.head.toString  + "&language=" + getLanguage(t))
      },
      {case t: HasHead if t.isVisible && !t.isInstanceOf[HTMLDefiniendum] =>
        if (t.resource.startsWith("var://") || t.resource.startsWith("varseq://")) {
          // TODO
        } else {
          overlay(t, "/:" + server.pathPrefix + "/fragment?" + t.head.toString + "&language=" + getLanguage(t),
            "/:" + server.pathPrefix + "/declaration?" + t.head.toString  + "&language=" + getLanguage(t))
        }
      },
      {case t : HTMLTopLevelTerm if !t.orig.isInstanceOf[HTMLDefiniendum] =>
        t.orig match {
          case h : HasHead if t.isVisible =>
            if (t.resource.startsWith("var://") || t.resource.startsWith("varseq://")) {
              // TODO
            } else {
              overlay(t, "/:" + server.pathPrefix + "/fragment?" + h.head.toString + "&language=" + getLanguage(t),
                "/:" + server.pathPrefix + "/declaration?" + h.head.toString  + "&language=" + getLanguage(t))
            }
          case _ =>
        }
        /*t.constant.foreach {c =>
          DocumentExtension.sidebar(t,{<span style="display:inline">Term {DocumentExtension.makeButton(
            "/:" + server.stexserver.pathPrefix + "/fragment?" + c.path + "&language=" + DocumentExtension.getLanguage(t),
            "/:" + server.stexserver.pathPrefix + "/declaration?" + c.path + "&language=" + DocumentExtension.getLanguage(t)
            ,server.stexserver.xhtmlPresenter.asXML(c.df.get,Some(c.path $ DefComponent)),false
          )}</span>} :: Nil)
        }*/
      } */
  )
}

trait DocumentExtension extends STeXExtension {
  def documentRules : List[PartialFunction[HTMLNode,Unit]]
}

object DocumentExtension extends STeXExtension {

  override def serverReturn(request: ServerRequest): Option[ServerResponse] = None /*  */
/*

  override def doHeader(head: HTMLNode,body: HTMLNode): Unit = {
  }


 */
}
