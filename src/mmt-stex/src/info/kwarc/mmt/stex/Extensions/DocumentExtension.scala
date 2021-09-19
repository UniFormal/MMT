package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.xhtml.{HTMLParser, MathMLTerm}
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
          Some(ServerResponse(doDocument(s).toString, "html"))
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
    filecontent.iterate(doE)
    filecontent
  }

  override def doHeader(head: HTMLNode,body: HTMLNode): Unit = {
    head.add(MMTSystem.getResourceAsString("mmt-web/stex/overlay.txt"))
    body.add(<div class="stexoverlay" id="stexMainOverlay" style="z-index:100;border-style:solid;position:fixed;top:10px;transition: width 0.2s smooth;background-color:white;">
        <table style="width:80ch;height:100%">
          <tr style="height:28px"><td style="text-align:right"><button onclick="stexOverlayOff('stexMainOverlay')">X</button></td></tr>
          <tr width="100%" height="100%"><td><iframe class="stexoverlayinner" id="stexoverlayinner" name="stexoverlayinner" onLoad="if (this.contentWindow.location.href=='about:blank') {} else {stexMainOverlayFade();};this.style.height=(this.contentWindow.document.body.offsetHeight+5) + 'px';" width="100%"
                                                     style="opacity:100; margin:0%; padding:0%; display:block;background-color:hsl(210, 20%, 98%)\">{HTMLParser.empty}</iframe>
          </td></tr>
        </table>
      </div>)
  }

  // TODO
  def getDocument(uri : String) : HTMLNode = {
    val rules = server.extensions.flatMap(_.rules)
    //val state = new ParsingState(,rules)
    uri match {
      case "http://mathhub.info/fomid/demo.xhtml" =>
        val state = new ParsingState(controller,rules)
        HTMLParser(MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml"))(state)
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

  def sidebar(elem : HTMLNode, content: List[Node]) = {
    val id = elem.state.generateId
    var e = if (elem.parent.exists(_.isVisible)) elem else elem.collectAncestor {
      case a if a.parent.exists(_.isVisible) => a
    }.getOrElse(elem)
    if (e.isMath) e = e.collectAncestor {
      case a if a.parent.exists(!_.isMath) => a
    }.getOrElse(e)
    e.parent.foreach(_.addBefore(<span style="display:inline">
      <label for={id} class="sidenote-toggle">{HTMLParser.empty}</label>
      <input type="checkbox" id={id} class="sidenote-toggle"/>
      <span class="sidenote" style="display:inline">{content}</span>
    </span>,e))
  }

  def overlay(elem : HTMLNode, urlshort : String,urllong : String) : Unit = {
    def criterion(n : HTMLNode) = n match {
      case t if t.attributes.isDefinedAt((HTMLParser.ns_stex,"arg")) => false
      case _ : HTMLText => true
      case t if t.getClass == classOf[HTMLNode] => true
      case t if t.getClass == classOf[MathMLTerm] => true
      case _ => false
    }
    if (elem.classes.contains("hasoverlay")) return
    elem.classes = "hasoverlay" :: elem.classes
    def pickelems(n : HTMLNode) : List[HTMLNode] = if (n.get()()().forall(criterion)) List(n) else
      n.children.filter(criterion).flatMap(pickelems)
    val id = elem.state.generateId
    val targets = pickelems(elem)
    val onhover = targets.indices.map(i => "document.getElementById('" + id + "_" + i + "').classList.add('stexoverlaycontainerhover')").mkString(";")
    val onout = targets.indices.map(i => "document.getElementById('" + id + "_" + i + "').classList.remove('stexoverlaycontainerhover')").mkString(";")
    val currp = elem.parent
    targets.zipWithIndex.foreach {
      case (txt: HTMLText,i) =>
        val newthis = txt.parent.map(_.addAfter(<span class="stexoverlaycontainer" style="display:inline"
                                            id={id + "_" + i}
                                            onmouseover={"stexOverlayOn('" + id + "','" + urlshort + "');" + onhover}
                                            onmouseout={"stexOverlayOff('" + id + "');" + onout}
                                            onclick={"stexMainOverlayOn('" + urllong + "')"}
        ></span>,txt))
        txt.delete
        newthis.foreach(_.add({
          if (txt.endswithWS && txt.startswithWS) new HTMLText(txt.state,"&nbsp;" + txt.text + "&nbsp;")
          else if (txt.endswithWS) new HTMLText(txt.state,txt.text + "&nbsp;")
          else if (txt.startswithWS) new HTMLText(txt.state,"&nbsp;" + txt.text)
          else txt
        }))
      case (e,i) =>
        e.classes ::= "stexoverlaycontainer"
        e.attributes((e.namespace, "onmouseover")) = "stexOverlayOn('" + id + "','" + urlshort + "');" + onhover
        e.attributes((e.namespace, "onmouseout")) = "stexOverlayOff('" + id + "');" + onout
        e.attributes((e.namespace, "onclick")) = "stexMainOverlayOn('" + urllong + "')"
        e.attributes((e.namespace,"id")) = id + "_" + i
    }
    val overlay = <span style="position:relative;display:inline"><iframe src=" " class="stexoverlay" id={id} onLoad='this.style.height=(this.contentWindow.document.body.offsetHeight+5) + "px";'>{HTMLParser.empty}</iframe></span>
    val after = if (elem.parent == currp) elem else elem.parent.get
    if (!after.isMath) {
      val p = after.parent.get
      p.addAfter(overlay, after)
    } else {
      val tm = after.collectAncestor {
        case n if n.parent.exists(!_.isMath) => n
      }
      tm.foreach(_.parent.foreach(_.addAfter(overlay, tm.get)))
    }
  }

  def makeButton(target : String,elem : Node) : Node =  // makesafe(XHTML(
      <span class="propbtn" style="display:inline" target="stexoverlayinner" onclick={"stexMainOverlayOn('" + target + "')"}>
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
