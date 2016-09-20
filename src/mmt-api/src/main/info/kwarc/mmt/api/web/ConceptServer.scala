package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.archives.HTMLPresenter
import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation.{HTMLRenderingHandler, Presenter, RenderingResult}
import info.kwarc.mmt.api.refactoring.ArchiveStore
import info.kwarc.mmt.api.utils.{File, _}
import tiscaf.HLet

import scala.collection.immutable.List
import scala.util.Try
import scala.xml.Node

/**
  * Created by raupi on 11.09.16.
  */
class ConceptServer extends ServerExtension("concepts") {
  override val logPrefix = "concepts"

  lazy val alignments = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  // TODO ugly quick hack to get a not massively overflowing menu
  val alphabet = List('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t',
    'u','v','w','x','y','z','!')

  private var filebase : File = null
  private var menu = ""

  override def start(args: List[String]) = {
    menu = HTML.build(makeMenu)
    if (args.length > 1 && args.head.trim == "index") {
      filebase = File(args(1))
      val all = alignments.getConcepts
      alphabet.foreach(saveIndex)
      all foreach saveCon
    } else if (args.nonEmpty) filebase = File(args.head)
  }

  def saveIndex(c : Char): Unit = if(filebase!=null) {
    log("saving index " + c)
    val file = (filebase / FileNameEscaping(c.toString)).addExtension("html")
    File.write(file,doFullPage(List(c.toString)))
  }
  def saveCon(con : String): Unit = if(filebase!=null) {
    log("saving concept: " + con)
    val file = (filebase / "con" / FileNameEscaping(con.toLowerCase)).addExtension("html")
    File.write(file,doFullPage(List("con",con)))
  }

  private def makeMenu(h : HTML) = {
    import h._
    log("Constructing menu...")
    val all = alignments.getConcepts.sortWith((a,b) => a.toLowerCase < b.toLowerCase)
    val list = alphabet collect {
      case '!' if all.exists(c => !alphabet.init.contains(c.toLowerCase.head)) => "!"
      case a if all.exists(c => c.toLowerCase.head == a) => a.toString.toUpperCase
    }
    ul { list.foreach(s => li { a("/:concepts/" + s) { text {s} } }) }
    log("Done.")
  }

  def doHeader(implicit h : HTML) = {
    import h._
    table(attributes=List(("width","100%"))) {
      tr {
        td(attributes=List(("alignment","left"))) {
          div(cls = "ui-widget") {
            new Element("label").apply(attributes = List(("for","tags"))) { text {"Search:"} }
            new Element("input").apply(id="tags") {}
          }
        }
        td(id="addalign",cls="addalign") {
          new Element("form").apply(attributes=List(("action","/:concepts/add"))) {
            text {"Add URI: "}
            new Element("input").apply(attributes=List(("type","text"),("name","URI"),("value","http://en.wikipedia.org/wiki/Cartesian_product"))) {}
            text {"  to Concept: "}
            new Element("input").apply(attributes=List(("type","text"),("name","concept"),("value","Cartesian Product"))) {}
            new Element("input").apply(attributes=List(("type","submit"),("value","Submit"))) {}
          }
        }
      }
    }

  }

  def doIndexPage(h : HTML, l : String) = {
    import h._
    if (!(l.length==1)) text { "Unknown index: " + l }
    else {
      h1 { text { l } }
      val ls = if (l=="!") alignments.getConcepts.filter(c => !alphabet.init.exists(ch => c.toLowerCase.startsWith(ch.toString)))
        else alignments.getConcepts.filter(_.toLowerCase.startsWith(l))
      ul { ls.sortWith((a,b) => a.toLowerCase < b.toLowerCase).foreach(s => li { a("/:concepts/con/"+ URLEscaping.apply(s)) { text {s} }}) }
    }
  }

  lazy val concstring = alignments.getConcepts.map(s => "\"" + s + "\"").mkString(",")

  private def doFullPage(path : List[String]) = HTML.build(h => {
    import h._
    html(attributes = List(("xmlns","http://www.w3.org/1999/xhtml"))) {
      head {
        literal(htmlhead) // See at the bottom
        new Element("script").apply() { literal{
          """  $( function() {
                     |    var availableTags = [""" +
          "\"None\",\"test1\",\"test2\"" + //concstring +
            """];
                $( "#tags" ).autocomplete({
                  source: availableTags
                });
              } );"""
        } }
      }
      h.body {
        div(id="header") {
          doHeader(h)
        }
        div(id="main") {
            table(id="layouttable") { tr {
              td(cls="layouttablecell",attributes = List(("width","10%")))  { div(cls="cellcontent") {
                literal { menu } }
              }
              td(cls="layouttablecell") { div(cls="cellcontent") {div(cls="conceptmain") {
                if (path.nonEmpty && path.head != "con") {
                  doIndexPage(h,path.head.toLowerCase)
                }
                else if (path.nonEmpty && path.head=="con" && path.length>=2) {
                  val con = URLEscaping.unapply(path(1).trim)
                  makeConceptPage(con,h)
                } else if (path.isEmpty) {
                  doIndexPage(h,"a")
                } else text {
                  log("CALL malformed: /" + path.mkString("/"))
                  "Unknown URL: /" + path.mkString("/")
                }
              }}}
            }}
        }
      }
    }
  })

  def apply(path: List[String], query: String, body: Body, session: Session) : HLet = if (path == List("add") && query != "") {
    log("Query: " + query)
    if (!query.startsWith("URI=") || !query.contains("&concept=")) Server.TextResponse("Malformed Query") else {
      val (uri,con) = (query.split('&').head.drop(4).trim,query.split('&')(1).replace("concept=","").trim)
      val all = alignments.getConcepts
      if (alignments.getConceptAlignments(con).map(_.toString.replace("http://","").replace("https://","")).contains(uri)) {
        return Server.TextResponse("URI " + uri + " already aligned with \"" + con + "\"!")
      }
      val ref = Try(LogicalReference(Path.parseMS(uri,NamespaceMap.empty))).getOrElse(PhysicalReference(URI(uri)))
      val alig = ConceptAlignment(ref,con)
      if (!all.contains(con)) {
        menu = HTML.build(makeMenu)
        // saveIndex(con.head.toLower)
      }
      alignments.addNew(alig)
      if (filebase!=null) {
        // saveCon(con)
      }
      log("Added URI " + ref + " to concept: " + con)
      Server.TextResponse("Added URI " + ref + " to concept: " + con + "\nTHANK YOU FOR CONTRIBUTING!")
    }
  } else {
    val ret = if (filebase!=null) {
      val file = path.foldLeft(filebase)((f,s) => f / FileNameEscaping.apply(URLEscaping.unapply(s).toLowerCase)).addExtension("html")
      if (file.exists) {
        log("CALL Saved file " + file)
        File.read(file)
      }
      else {
        if (path.length == 1 && path.head.length==1) {
          log("CALL constructing index " + path.head.head.toLower)
          saveIndex(path.head.head.toLower)
          File.read(file)
        } else if(path.length>1 && path.head == "con") {
          log("CALL constructing concept " + URLEscaping.unapply(path(1)))
          saveCon(URLEscaping.unapply(path(1)))
          File.read(file)
        } else "path malformed: " + path.mkString("/")
      }
    } else doFullPage(path)
    log("Call returned.")
    Server.XmlResponse(ret)
  }

  lazy val presenter = controller.extman.get(classOf[Presenter],"html").get.asInstanceOf[HTMLPresenter]
  lazy val archives : ArchiveStore = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }

  def makeConceptPage(con : String, h : HTML) = {
    val refs = alignments.getConceptAlignments(con)
    val altnames = refs.collect {
      case ConceptReference(s) if s!= con => s
    }
    val uris = refs collect {case uri : URIReference => uri}
    val smglom = uris collect {
      case LogicalReference(pat) if pat.toString.contains("smglom") && Try(controller.get(pat)).isSuccess => pat
    }
    val formals = uris collect {
      case LogicalReference(pat) if !smglom.contains(pat) => pat
    }
    val informals = (uris collect {
      case PhysicalReference(uri) => uri.toString.replace("http://","").replace("https://","")
    }).distinct
    import h._
    implicit val rh = new HTMLRenderingHandler(h)
      span {
        h1() {text{con}}
        if (altnames.nonEmpty) div {
          text {"(See also: "}
          altnames.init.foreach(n => {
            a("/:concepts/con/" + URLEscaping(n)) {text {n} }
            text {", "}
          })
          a("/:concepts/con/" + URLEscaping(altnames.last)) {text {altnames.last} }
          text {")"}
        }
        def doInformal(s : => Unit) = tr { td(cls="ext-table",attributes = List(("align","center"))) { s }}
        if (informals.nonEmpty) p {
          h3 { text { "External Resources:"} }
          div(id="externals") {
            table(id="ext-table",cls="ext-table",attributes = List(("width","90%"))) {
              informals.foreach(s =>
                if (s.contains("wikipedia.org")) doInformal(WikiExtractor(s, h))
                else if (s.contains("ncatlab.org")) doInformal(NLabExtractor(s, h))
                else if (s.contains("mathworld.wolfram.com")) doInformal(WolframExtractor(s, h))
                else if (s.contains("encyclopediaofmath.org")) doInformal(
                  EncyclopediaOfMathExtractor(s, h)
                )
                else if (s.contains("planetmath.org")) doInformal(PlanetMathExtractor(s, h))
                else doInformal(a("http://" + s) { text { s } })
              )
            }
          }
        }
        if (formals.nonEmpty) {
          h3 { text{ "Formal Libraries:" } }
          div(cls="document toggle-root inlineBoxSibling",id="formal") {
          table(id="formal-table",cls="formal-table",attributes = List(("width","90%"))) {
            val pairs = formals map (path => {
              val src = controller.getO(path)
              val arch = if (src.isDefined) archives.find(path).headOption.map(_.name) else None
              (arch, src, path)
            })
            pairs.filter(p => p._1.isDefined && p._2.isDefined) foreach { case (Some(arch), Some(src), _) => tr(cls="formal-table") {
              td {
                (new Element("b")) {
                  text {
                    arch
                  }
                }
              }
              td {
                presenter(src, false)
              }
            }
            }
            if (pairs.exists(p => p._1.isEmpty && p._2.isDefined)) tr {
              td {
                text {
                  "Unknown"
                }
              }
              td {
                table {
                  pairs.filter(p => p._1.isEmpty && p._2.isDefined) foreach { case (_, Some(src), _) => tr {
                    td {
                      presenter(src, false)
                    }
                  }
                  }
                }
              }
            }
            if (pairs.exists(p => p._1.isEmpty && p._2.isEmpty)) tr {
              td {
                text {
                  "Not available"
                }
              }
              td {
                ul {
                  pairs.filter(p => p._2.isEmpty && p._2.isEmpty) foreach {
                    case (_, _, path) => li {
                      text {
                        path.toString
                      }
                    }
                  }
                }
              }
            }
          }
          }
        }
      }
  }
  private def doFormal(p : Path) = {
    try {
      val dec = controller.get(p)
      val h = new HTMLBuilder
      implicit val rh = new HTMLRenderingHandler(h)
      presenter(dec, false)
      h.result
    } catch {
      case e: Exception => <a href={p.toString} target="_blank">
        {p.toString}
      </a> <br/>
    }
  }

  val htmlhead = """<meta charset="UTF-8" />
                   <!-- <link rel="stylesheet" type="text/css" href="/css/omdoc/omdoc-default.css"/> -->
                         <title>MMT Web Server</title>
                         <script type="text/javascript" src="/script/jquery/jquery.js"></script>
                         <link rel="stylesheet" type="text/css" href="/css/bootstrap/css/bootstrap.min.css"></link>
                         <link rel="stylesheet" type="text/css" href="/css/mmt.css" />
                         <link rel="stylesheet" type="text/css" href="/css/concepts.css" />
                         <link rel="stylesheet" type="text/css" href="/css/JOBAD.css" />
                         <link rel="stylesheet" type="text/css" href="/css/jquery/jquery-ui.css" />
                   <!-- <link rel="stylesheet" type="text/css" href="/css/jquery/ui.base.css"/> -->
                         <link rel="shortcut icon" href="/mmt2.png" />
                         <!--
                         <script src="/script/codemirror2/lib/codemirror.js"></script>
                         <link rel="stylesheet" href="script/codemirror2/lib/codemirror.css" />
                         <script src="/script/codemirror2/mode/lf/lf.js"></script>
                         -->
                         <script type="text/javascript" src="/script/jquery/jquery-ui.js"></script>
                         <script type="text/javascript" src="/script/tree/jquery.hotkeys.js"></script><!-- used by  stree -->
                         <script type="text/javascript" src="/script/tree/jquery.jstree.js"></script>

                         <!-- incremental search by Kazuhisa Nakasho
                         <script type="text/javascript" src="/script/incsearch/incsearch.js"></script> -->
                         <script type="text/javascript" src="/script/incsearch/treeview.js"></script>
                         <link rel='stylesheet' href='/css/incsearch/jstree.css'/>
                         <link rel='stylesheet' href='/css/incsearch/index.css'/>
                         <link rel='stylesheet' href='/css/incsearch/incsearch.css'/>
                         <link rel='stylesheet' href='/css/incsearch/treeview.css'/>

                   <!-- Core JS API for MMT interaction -->
                         <script type="text/javascript" src="/script/mmt/mmt-js-api.js"></script>
                   <!-- JOBAD Deps -->
                         <script type="text/javascript" src="/script/jobad/deps/underscore-min.js"></script>
                   <!-- JOBAD -->
                         <script type="text/javascript" src="/script/jobad/JOBAD.js"></script>
                   <!-- JOBAD Services -->
                         <script type="text/javascript" src="/script/jobad/modules/hovering.js"></script>
                         <script type="text/javascript" src="/script/jobad/modules/interactive-viewing.js"></script>
                   <!-- browser-specific JS -->
                         <script type="text/javascript" src="/script/mmt/concepts.js"></script>"""
}
