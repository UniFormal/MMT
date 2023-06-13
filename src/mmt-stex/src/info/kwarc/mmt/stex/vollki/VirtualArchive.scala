package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.{DPath, ErrorLogger, GlobalName, Path, StructuralElement, utils}
import info.kwarc.mmt.api.archives.ArchiveLike
import info.kwarc.mmt.api.documents.{ArchiveLevel, Document, FolderLevel}
import info.kwarc.mmt.api.frontend.{Controller, Report}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.Include
import info.kwarc.mmt.api.utils.{FilePath, JSON, JSONArray, JSONObject, JSONString, URI}
import info.kwarc.mmt.stex.STeXServer
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser, HTMLText, SHTMLDefiniendum, SHTMLDefinition, SHTMLImportModule, SHTMLNode, SHTMLOMS, SHTMLParsingRule, SHTMLRule, SHTMLTheory, SHTMLUseModule, SemanticState}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.collection.mutable
import scala.util.Try

abstract class VirtualArchive extends ArchiveLike {
  lazy val url:String = properties("url")
  lazy val urlbase = URI(url).^
  val rootString = url
  val narrationBase = properties.get("narration-base").map(utils.URI(_)).getOrElse(utils.URI(url))
  val controller : Controller
  val report = controller.report
  protected val elements = mutable.HashMap.empty[Path,StructuralElement]
  override def load(path: Path)(implicit controller: Controller): Unit = elements.get(path) match {
    case Some(e) => controller.add(e)
    case None =>
  }
  def getIndex: List[JSONObject]
  def getDoc(fp:String) : List[HTMLNode]
  def getHeader(fp:String) : List[HTMLNode]

  def downloadAsString(url : String) = {
    val connection = new java.net.URL(url).openConnection.asInstanceOf[javax.net.ssl.HttpsURLConnection]
    connection.setRequestMethod("GET")
    val inputStream = connection.getInputStream
    val html = scala.io.Source.fromInputStream(inputStream).mkString
    inputStream.close()
    html
  }
  def importAll:Unit

  case class IndexNode(label: String, fp: String, children: List[IndexNode]) {
    def toJSON: JSONObject =
      JSONObject(
        ("label", JSONString(label)),
        ("children", JSONArray(children.map(_.toJSON): _*)),
        ("link", JSONString(uglyhack(fp)))
      )

    def files : List[URI] = if (fp == "") children.flatMap(_.files) else
      {
        fp.replace(".xhtml",".html").split('/').foldLeft(urlbase)((a,b) => a / b)
      } :: children.flatMap(_.files)

    private def uglyhack(fp: String): String = {
      val pairstr = if (fp == "") id else id + "&filepath=" + fp.replace(".html", ".xhtml")
      "var iframe = document.getElementById(\"targetiframe\");iframe.src = " +
        "\"/:sTeX/fulldocument?archive=" + pairstr + "\";document.getElementById(\"archive\").innerHTML" +
        " = \"<b>" + id + "</b>\";document.getElementById(\"contentpath\").innerHTML = \"<code>" + fp + "</code>\"" +
        ";var newtabbutton = document.getElementById(\"newtabbutton\");newtabbutton.href=(\"/:sTeX/fulldocument?archive=" +
        pairstr + "\");newtabbutton.style.display=\"inline\""
    }
  }
}


class JupyterBookArchive(val controller:Controller,val properties: mutable.Map[String, String]) extends VirtualArchive {

  private val htmap = mutable.HashMap.empty[String,(List[HTMLNode],List[HTMLNode])]

  private val name_to_uri = mutable.HashMap.empty[String,GlobalName]
  private val nameoccs = mutable.HashMap.empty[String,GlobalName => Unit]

  lazy val server = controller.extman.get(classOf[STeXServer]) match {
    case Nil =>
      val s = new STeXServer
      controller.extman.addExtension(s)
      s
    case s :: _ =>
      s
  }
  lazy val eh = new ErrorLogger(controller.report)
  case class DummyNode(n : HTMLNode) extends SHTMLNode(n) {
    def copy = DummyNode(n.copy)
  }
  private def URIToModule(f:URI) = DPath(f.^) ? f.path.last.replace(".html","").replace(".md","")
  def importAll = index.flatMap(_.files).foreach(importOne)
  def importOne(f:URI) : Unit = {
    val dictionary = mutable.HashMap.empty[String,GlobalName]
    def addToDict(th:Theory) : Unit = {
      th.getDeclarations.foreach {
        case c: info.kwarc.mmt.api.symbols.Constant =>
          dictionary(c.name.toString) = c.path
        case Include(i) =>
          controller.getO(i.from).foreach {
            case t: Theory => addToDict(t)
            case _ =>
          }
        case _ =>
      }
    }
    val rules = List(
      new SHTMLRule(-100) {
        def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = if (attrs.nonEmpty && !n.isInstanceOf[DummyNode]) {
          println("Missing: " + attrs.map(_._1).mkString(", "))
          None
        } else None
      },
      new SHTMLRule() {
        override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
          if (n.label == "article") {
            val mp = URIToModule(f)
            Some(new SHTMLTheory(mp, n))
          } else None
        }
      },
      SHTMLParsingRule("usemodule", (str, n, _) => {
        val mp = Try(Path.parseM(str)).toOption.getOrElse {
          val uri = f.resolve(str)
          val mp = URIToModule(uri)
          controller.getO(mp) match {
            case Some(_) => mp
            case None =>
              importOne(uri)
              mp
          }
        }
        controller.getO(mp) match {
          case Some(th: Theory) => addToDict(th)
          case _ =>
            println("Missing: " + mp)
            print("")
        }
        SHTMLUseModule(mp, n)
      }),
      SHTMLParsingRule("importmodule", (str, n, _) => {
        val mp = if (str.count(_ == '?') == 2) { // temporary hack
          Path.parseM(str.replaceFirst("\\?","/"))
        } else Try(Path.parseM(str)).toOption.getOrElse {
          val uri = f.resolve(str)
          val mp = URIToModule(uri)
          controller.getO(mp) match {
            case Some(_) => mp
            case None =>
              importOne(uri)
              mp
          }
        }
        controller.getO(mp) match {
          case Some(th: Theory) => addToDict(th)
          case _ =>
            println("Missing: " + mp)
            print("")
        }
        n.plain.attributes.remove((HTMLParser.ns_shtml, "importmodule"))
        n.plain.attributes((HTMLParser.ns_shtml,"import")) = mp.toString
        SHTMLImportModule(mp, n)
      }),
      SHTMLParsingRule("definition", (_, n, _) => SHTMLDefinition(n), -30),
      SHTMLParsingRule("definiendum", (s, n, _) => {
        Try(Path.parseS(s)).toOption.orElse(dictionary.get(s)) match {
          case Some(p) =>
            SHTMLDefiniendum(p, n)
          case _ =>
            val ret = DummyNode(n)
            nameoccs(s) = gn => {
              n.plain.attributes((HTMLParser.ns_shtml, "definiendum")) = gn.toString
              ret.replace(SHTMLDefiniendum(gn, n))
            }
            DummyNode(n)
        }
      }),
      SHTMLParsingRule("term", (s, n, _) => {
        s match {
          case "OMID" | "complex" =>
            val heads = n.plain.attributes((HTMLParser.ns_shtml, "head"))
            Try(Path.parseS(heads)).toOption.orElse(dictionary.get(s)) match {
              case Some(p) =>
                SHTMLOMS(n)
              case _ =>
                val dummy = DummyNode(n)
                nameoccs(heads) = gn => {
                  n.plain.attributes((HTMLParser.ns_shtml, "head")) = gn.toString
                  val ret = SHTMLOMS(n)
                  dummy.replace(ret)
                  ret
                }
                dummy
            }
          case _ =>
            ???
        }
      }),
    )
    try {
      htmap.getOrElseUpdate(f.toString, {
        val content = downloadAsString(f.toString.replace(".md",".html"))
        val html = HTMLParser(content)(new SemanticState(server,rules,eh,DPath(f)) {
          override def add(se: StructuralElement): Unit = {
            super.add(se)
            se match {
              case d : Document =>
                val top = d.path.^^
                elements.getOrElseUpdate(top, {
                  val nd = new Document(top,level = ArchiveLevel)
                  controller.add(nd)
                  nd
                })
                d.path.name.prefixes.foreach {p =>
                  elements.getOrElseUpdate(top / p, {
                    val nd = new Document(top / p,level = FolderLevel)
                    controller.add(nd)
                    nd
                  })
                }
              case _ =>
            }
            elements(se.path) = se
          }
        })//(new ParsingState(controller, rules))
        doHeadBody(html, f)
      })
    } catch {
      case _: java.io.IOException | _: info.kwarc.mmt.stex.STeXError =>
    }
  }

  private def doHeadBody(html:HTMLNode,baseurl:URI) : (List[HTMLNode],List[HTMLNode]) = {
    val head = html.get("head")()().head.children
    head.foreach(_.delete)
    head.foreach(_.iterate { n =>
      n.plain.attributes.foreach {
        case (("", rs), url) if (rs == "href" || rs == "src" || rs == "data-url_root") && !url.startsWith("http") =>
          n.plain.attributes((n.namespace, rs)) = baseurl.resolve(url).toString
        case _ =>
      }
    })
    (head, html.get("article")()().head.children)
  }
  override def getDoc(fp: String): List[HTMLNode] =
    getHTMLs(fp)._2
  override def getHeader(fp: String): List[HTMLNode] = getHTMLs(fp)._1
  private def getHTMLs(fp:String): (List[HTMLNode],List[HTMLNode]) = {
    val geturl = (urlbase / fp.replace(".xhtml", ".html")).toString
    try {
      htmap.getOrElseUpdate(geturl, {
        val html = HTMLParser(downloadAsString(geturl))(new ParsingState(controller, Nil))
        val baseurl = fp.split('/').foldLeft(urlbase)((u,s) => u / s)
        doHeadBody(html,baseurl)
      })
    } catch {
      case t : Throwable =>
        t.printStackTrace()
        (Nil,Nil)
    }
  }

  def getIndex = index.map(_.toJSON)
  lazy val index = {
    val html = downloadAsString(url)
    val ht = HTMLParser(html)(new ParsingState(controller, Nil))
    val lists = ht.get("div")()("navbar-nav").head.children.tail.grouped(2).collect { case List(a, b) => (a, b) }.toList
    var ret: List[IndexNode] = Nil

    def getChildren(elem: HTMLNode): List[IndexNode] = elem.children.flatMap(e => e.children match {
      case List(e) if e.label == "a" =>
        val label = (e.children.collect { case ht: HTMLText => ht.text }).mkString
        val filepath = e.plain.attributes.getOrElse((e.namespace, "href"), "")
        IndexNode(label,filepath,Nil) :: Nil
      case e :: rest if e.label == "a" && rest.nonEmpty && rest.last.label == "ul" =>
        val label = (e.children.collect { case ht: HTMLText => ht.text }).mkString
        val children = getChildren(rest.last)
        IndexNode(label,"",children) :: Nil
      case _ => Nil
    })

    lists.foreach {
      case (ttl, chs) if ttl.label == "p" && chs.label == "ul" =>
        val name = ttl.children.flatMap(_.children.collect { case t: HTMLText => t.text }).mkString
        val children = getChildren(chs)
        ret ::= IndexNode(name.mkString,"",children)
      case _ =>
    }
    ret
  }
}