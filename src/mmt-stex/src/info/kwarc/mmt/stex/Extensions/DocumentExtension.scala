package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.xhtml.{ArgumentAnnotation, OMDocAnnotation, ToScript, XHTML, XHTMLAnnotation, XHTMLNode, XHTMLOMDoc, XHTMLParsingState, XHTMLText}

import scala.xml.{Elem, Node}

trait DocumentExtension extends STeXExtension {
  def documentRules : List[PartialFunction[XHTMLAnnotation,Unit]]
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
    def doE(e : XHTMLNode) : Unit = e.getAnnotations.foreach(a => docrules.foreach(r => r.unapply(a)))
    filecontent.iterate(doE)
    filecontent
  }

  override def doHeader(head: XHTMLNode,body: XHTMLNode): Unit = {
    XHTML.applyString("<html><head>" + MMTSystem.getResourceAsString("mmt-web/stex/overlay.txt") + "</head></html>").children.head.children.foreach(head.add)
    body.add(XHTML.apply(
      <div class="stexoverlay" id="stexMainOverlay" style="z-index:100;border-style:solid;position:fixed;top:10px;transition: width 0.2s smooth;background-color:white;">
        <table style="width:80ch;height:100%">
          <tr style="height:28px"><td style="text-align:right"><button onclick="stexOverlayOff('stexMainOverlay')">X</button></td></tr>
          <tr width="100%" height="100%"><td><iframe class="stexoverlayinner" id="stexoverlayinner" name="stexoverlayinner" onLoad="if (this.contentWindow.location.href=='about:blank') {} else {stexMainOverlayFade();};this.style.height=(this.contentWindow.document.body.offsetHeight+5) + 'px';" width="100%"
                                                     style="opacity:100; margin:0%; padding:0%; display:block;background-color:hsl(210, 20%, 98%)\">{XHTML.empty}</iframe>
          </td></tr>
        </table>
      </div>)
    )
  }

  // TODO
  def getDocument(uri : String) : XHTMLNode = {
    val rules = server.extensions.flatMap(_.xhtmlRules)
    val state = new XHTMLParsingState(rules)
    uri match {
      case "http://mathhub.info/fomid/demo.xhtml" =>
        XHTML.applyString(MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml"),Some(state))
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
              val cnt = a.properties.collectFirst {
                case ("description",d) if (a.root / "META-INF" / d).exists() =>
                  XHTML.applyString(File.read(a.root / "META-INF" / d),Some(state))
                case ("description",d) if (a.root / "meta-inf" / d).exists() =>
                  XHTML.applyString(File.read(a.root / "meta-inf" / d),Some(state))
                case ("teaser",t) =>
                  XHTML.applyString("<div>"+t+"</div>",Some(state))
              }
              bd.add(<b style="text-align: center">{a.id}</b>)
              cnt.foreach(bd.add)
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
              if (fn.exists() && fn.isDirectory) {
                if ((fn / "index.xhtml").exists()) {
                  val ret = XHTML.applyString(File.read(fn / "index.xhtml"),Some(state))
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
                val ret = XHTML.applyString(File.read(fn),Some(state))
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

  def sidebar(elem : XHTMLNode, content: List[Node],atend : Boolean = false) = {
    def pickelem(e : XHTMLNode) : Option[XHTMLNode] = {
      e.collectFirstAncestor{case a if a.getAnnotations.exists(_.isInstanceOf[ToScript]) => a}.foreach{a => return Some(a)}
      if (atend) e.children.reverse else e.children}.collectFirst {
        case ie if !ie.isEmpty && !ie.isMathML && !ie.classes.contains("ltx_rdf") =>
          pickelem(ie).getOrElse(ie)
        case ie if !ie.isMathML && !ie.classes.contains("ltx_rdf") =>
          ie
      }
    val id = elem.top.generateId
    val annot = XHTML(
      <span>
        <label for={id} class="sidenote-toggle">{XHTML.empty}</label>
        <input type="checkbox" id={id} class="sidenote-toggle"/>
        <span class="sidenote">{content}</span>
      </span>
    ).children.map(makesafe)
    val e = pickelem(elem).getOrElse(elem)
    if (e.isMathML) {
      val tm = e.getTopMath
      tm.parent.foreach(p => annot.foreach(p.addAfter(_, tm)))
    } else {
      e.parent.foreach(p => annot.foreach(p.addAfter(_,e)))
    }
  }

  def overlay(elem : XHTMLNode, urlshort : String,urllong : String) = {
    def criterion(n : XHTMLNode) = {
      n.getAnnotations.isEmpty || n.isInstanceOf[XHTMLText] || !n.getAnnotations.exists(_.isInstanceOf[OMDocAnnotation])
    } && !n.getAnnotations.exists(_.isInstanceOf[ArgumentAnnotation])
    def pickelems(e: XHTMLNode): List[XHTMLNode] = if (
      e.get()()().forall(criterion)) List(e) else {
      e.children.filter(criterion).flatMap(pickelems)
    }

    val t = elem.top
    val id = t.generateId
    val overlay = XHTML.apply(<span style="position:relative"><iframe src=" " class="stexoverlay" id={id} onLoad='this.style.height=(this.contentWindow.document.body.offsetHeight+5) + "px";'>{XHTML.empty}</iframe></span>)
    val currp = elem.parent
    val targets = pickelems(elem)
    val onhover = targets.indices.map(i => "document.getElementById('" + id + "_" + i + "').classList.add('stexoverlaycontainerhover')").mkString(";")
    val onout = targets.indices.map(i => "document.getElementById('" + id + "_" + i + "').classList.remove('stexoverlaycontainerhover')").mkString(";")
    targets.zipWithIndex.foreach {
      case (txt: XHTMLText,i) =>
        val newthis = XHTML(<span class="stexoverlaycontainer"
                                        id={id + "_" + i}
                                        onmouseover={"stexOverlayOn('" + id + "','" + urlshort + "');" + onhover}
                                        onmouseout={"stexOverlayOff('" + id + "');" + onout}
                                        onclick={"stexMainOverlayOn('" + urllong + "')"}
        ></span>)
        txt.parent.foreach(_.addAfter(newthis,txt))
        txt.delete
        newthis.add(txt)
      case (e,i) =>
        e.attributes(("", "class")) = e.attributes.get(("", "class")) match {
          case Some(s) => s + " stexoverlaycontainer"
          case _ => "stexoverlaycontainer"
        }
        e.attributes(("", "onmouseover")) = "stexOverlayOn('" + id + "','" + urlshort + "');" + onhover
        e.attributes(("", "onmouseout")) = "stexOverlayOff('" + id + "');" + onout
        e.attributes(("", "onclick")) = "stexMainOverlayOn('" + urllong + "')"
        e.attributes(("","id")) = id + "_" + i
    }
    val after = if (elem.parent == currp) elem else elem.parent.get
    if (!after.isMathML) {
      val p = after.parent.get
      p.addAfter(overlay, after)
    } else {
      val tm = after.getTopMath
      tm.parent.foreach(_.addAfter(overlay, tm))
    }
  }

  def makeButton(target : String,elem : Node) = makesafe(XHTML(
      <span class="propbtn" target="stexoverlayinner" onclick={"stexMainOverlayOn('" + target + "')"}>
        {elem}
      </span>
  ))

  def makePostButton(elem : Node, target : String,data : (String,String)*) = makesafe(XHTML(
    <form method="post" action={target} class="inline" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner" style="display:none">
      {data.map{p => <input type="hidden" name={p._1} value={p._2} class="inline"/>}}
      <span onclick="this.closest('form').submit();" type="submit" class="propbtn">
        {elem}
      </span>
    </form>))

  def makesafe(xh : XHTMLNode) = {
    new XHTMLNode() {
      override def node = xh.node
    }
  }
}
