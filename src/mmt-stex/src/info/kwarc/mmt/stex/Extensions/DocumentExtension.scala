package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.xhtml.{HTMLArg, HTMLComp, HTMLLanguageComponent, HTMLParser, HasHead, NotationComponent}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}

import scala.xml.parsing.XhtmlParser
import scala.xml.{Elem, Node}

trait DocumentExtension extends STeXExtension {
  def documentRules : List[PartialFunction[HTMLNode,Unit]]
}

object DocumentExtension extends STeXExtension {

  override def serverReturn(request: ServerRequest): Option[ServerResponse] = request.path.lastOption match {
    case Some("document") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty Document path","txt"))
        case s =>
          Some(ServerResponse(doDocument(s).toString, "text/html"))
      }
    case Some("fulldocument") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty Document path","txt"))
        case s =>
          var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
          html = html.replace("CONTENT_URL_PLACEHOLDER","/:" + server.pathPrefix + "/document?" + s)
          html = html.replace("BASE_URL_PLACEHOLDER","")
          html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
          Some(ServerResponse(html, "text/html"))
      }
    case _ => None
  }

  def doDocument(uri : String) = {
    val exts = server.extensions
    val filecontent = getDocument(uri)
    val docrules = exts.collect {
      case e : DocumentExtension =>
        e.documentRules
    }.flatten
    def doE(e : HTMLNode) : Unit = docrules.foreach(r => r.unapply(e))
    val body = filecontent.get("body")()().head
    body.iterate(doE)
    body
  }

  override def doHeader(head: HTMLNode,body: HTMLNode): Unit = {
    //head.add(MMTSystem.getResourceAsString("mmt-web/stex/overlay.txt"))
    //body.add(MMTSystem.getResourceAsString("mmt-web/stex/htmlfragments/overlaymain.xml"))
  }

  def getDocument(uri : String) : HTMLNode = {
    val rules = server.extensions.flatMap(_.rules)
    //val state = new ParsingState(,rules)
    uri match {
      case s if s.startsWith("group=") =>
        val grp = s.drop(6)
        val doc = server.emptydoc
        doc._2.add(<b style="text-align: center">{"Group: " + grp}</b>)
        doc._1
      case s if s.startsWith("archive=") =>
        val (id,path) = {
          s.drop(8).split('&') match {
            case Array(s) => (s,FilePath(Nil))
            case Array(s,r) if r.startsWith("filepath=") =>
              (s,FilePath(r.drop(9).split('/').toList))
            case _ =>
              print("")
              ???
          }
        }
        if (path.isEmpty) {
          val (doc,bd) = server.emptydoc
          controller.backend.getArchive(id) match {
            case None =>
              bd.add(<b>{"No archive with id " + id + " found!"}</b>)
            case Some(a) =>
              bd.add(<b style="text-align: center">{a.id}</b>)
              a.properties.collectFirst {
                case ("description",d) if (a.root / "META-INF" / d).exists() =>
                 bd.add(File.read(a.root / "META-INF" / d))
                case ("description",d) if (a.root / "meta-inf" / d).exists() =>
                  bd.add(File.read(a.root / "meta-inf" / d))
                case ("teaser",t) =>
                  bd.add("<div>"+t+"</div>")
              }
          }
          doc
        } else {
          controller.backend.getArchive(id) match {
            case None =>
              val (doc, bd) = server.emptydoc
              bd.add(<b>
                {"No archive with id " + id + " found!"}
              </b>)
              doc
            case Some(a) =>
              val top = a / RedirectableDimension("xhtml")
              val fn = top / path
              val state = new ParsingState(controller,rules)
              if (fn.exists() && fn.isDirectory) {
                if ((fn / "index.xhtml").exists()) {
                  val ret = HTMLParser(fn / "index.xhtml")(state)
                  server.doHeader(ret)
                  ret
                } else {
                  val (doc, bd) = server.emptydoc
                  bd.add(<b>
                    {id + "/" + path}
                  </b>)
                  doc
                }
              } else if (fn.exists()) {
                val ret = HTMLParser(fn)(state)
                server.doHeader(ret)
                ret
              }
              else {
                val (doc, bd) = server.emptydoc
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

  def getLanguage(elem:HTMLNode) = {
    val top = getTop(elem)
    top.get()((HTMLParser.ns_html,"property","stex:language"))().headOption match {
      case Some(l : HTMLLanguageComponent) => l.resource
      case _ => ""
    }
  }

  private def getTop(elem:HTMLNode) : HTMLNode = if (elem.label == "body") elem else elem.parent match {
    case Some(p) => getTop(p)
    case _ => elem
  }

  def sidebar(elem : HTMLNode, content: List[Node]) = {
    def parent(e : HTMLNode) : Option[(HTMLNode,HTMLNode)] = e.parent match {
      case Some(p) if !p.isMath => Some((p,e))
      case Some(p) => parent(p)
      case None => None
    } //if (e.isMath) parent(e.parent.get) else e
    parent(elem).foreach {case (p,c) =>
      p.addBefore(<div class="sidebar">{content}</div>,c)
    }
    //val sidenotes = getTop(elem).get()()("sidenote-container").headOption
    //sidenotes.foreach(_.add(<div>{content}</div>))
  }/*{
    val id = elem.state.generateId
    val side = <span id={id} class="sidenote-container"><span class="sidenote-container-b"><small class="sidenote">{content}</small></span></span>
    var e = if (elem.parent.exists(_.isVisible)) elem else elem.collectAncestor {
      case a if a.parent.exists(_.isVisible) => a
    }.getOrElse(elem)
    if (e.isMath) e = e.collectAncestor {
      case a if a.parent.exists(!_.isMath) => a
    }.getOrElse(e)
    e.parent.foreach(_.addBefore(side,e))
  }*/

  def overlay(elem : HTMLNode, urlshort : String,urllong : String) : Unit = {
    if (elem.classes.contains("overlay-parent")) return
    val id = elem.hashCode().toString
    elem.attributes((elem.namespace,"id")) = id
    elem.classes ::= "hasoverlay-parent"
    //elem.classes ::= "hasoverlay" //:: elem.classes
    def pickelems(n : HTMLNode,inarg:Boolean = false) : List[HTMLNode] = n match {
      case comp : NotationComponent => List(comp)
      case a : HTMLArg => a.children.flatMap(pickelems(_,true))
      case _ : HasHead if inarg => Nil
      case o => o.children.flatMap(pickelems(_,inarg))
    }
    val targets = pickelems(elem)
    targets.foreach{ e =>
      e.classes ::= "group-highlight"
      e.attributes((e.namespace,"data-highlight-parent")) = id
      e.attributes((elem.namespace,"data-overlay-link-hover")) = urlshort
      e.attributes((elem.namespace,"data-overlay-link-click")) = urllong
    }
  }

  def makeButton(urlshort : String,urllong:String,elem : Node, withclass : Boolean = true) : Node =  // makesafe(XHTML(
      <span class={if (withclass) "propbtn" else ""} style="display:inline" data-overlay-link-click={urllong} data-overlay-link-hover={urlshort}>
        {elem}
      </span>
  //))

  def makePostButton(elem : Node, target : String,data : (String,String)*) : Node = // makesafe(XHTML(
    <form method="post" action={target} class="inline" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner" style="display:none">
      {data.map{p => <input type="hidden" name={p._1} value={p._2} class="inline"/>}}
      <span onclick="this.closest('form').submit();" type="submit" class="propbtn" style="display:inline">
        {elem}
      </span>
    </form>
  //))

  def makesafe(xh : HTMLNode) = {
    new HTMLNode(xh.state,xh.namespace,xh.label) {
      override def node = xh.node
      override def toString: String = xh.toString
    }
  }
}
