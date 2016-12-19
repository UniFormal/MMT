package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.presentation.HTMLPresenter
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation.{HTMLRenderingHandler, Presenter, RenderingResult}
import info.kwarc.mmt.api.refactoring.ArchiveStore
import info.kwarc.mmt.api.utils.{File, _}
import tiscaf.HLet

import scala.collection.immutable.List
import scala.util.Try

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

  private var menu = ""
  private var conlist : List[String] = Nil

  override def start(args: List[String]) = {
    conlist = alignments.getConcepts.sortWith((a,b) => a.toLowerCase < b.toLowerCase)
    menu = HTML.build(makeMenu)
  }

  private def makeMenu(h : HTML) = {
    import h._
    log("Constructing menu...")
    val list = alphabet collect {
      case '!' if conlist.exists(c => !alphabet.init.contains(c.toLowerCase.head)) => "!"
      case a if conlist.exists(c => c.toLowerCase.head == a) => a.toString.toUpperCase
    }
    ul { list.foreach(s => li { a(":concepts?page=" + s) { text {s} } }) }
    log("Done.")
  }

  def doHeader(implicit h : HTML) = {
    import h._
    table(attributes=List(("width","100%"))) {
      tr {
        td(attributes=List(("alignment","left"))) {
          div(cls = "ui-widget") {
            // new Element("label").apply(attributes = List(("for","mysearch"))) { text {"Search:"} }
            literal {"""<input id="mysearch"/>"""}
            new Element("input").apply(onclick = "window.location=\"?con=\" + document.getElementById(\"mysearch\").value",
              attributes=List(("type","submit"),("value","Search"))) {}
          }
        }
        td(id="addalign",cls="addalign") {
          new Element("form").apply(attributes=List(("action",":concepts/add"))) {
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
      val ls = if (l=="!") conlist.filter(c => !alphabet.init.exists(ch => c.toLowerCase.startsWith(ch.toString)))
        else conlist.filter(_.toLowerCase.startsWith(l))
      ul { ls.sortWith((a,b) => a.toLowerCase < b.toLowerCase).foreach(s => li { a(":concepts?con="+ URLEscaping.apply(s)) { text {s} }}) }
    }
  }

  private def doFullPage(path : List[String]) = HTML.build(h => {
    import h._
    html(attributes = List(("xmlns","http://www.w3.org/1999/xhtml"))) {
      head {
        literal(htmlhead) // See at the bottom
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

  def apply(path: List[String], query: String, body: Body, session: Session) : HLet =
    if (path == List("add") && query != "") {
      log("Query: " + query)
      if (!query.startsWith("URI=") || !query.contains("&concept=")) Server.TextResponse("Malformed Query")
      else {
        val (uri, con) = (query.split('&').head.drop(4).trim, query.split('&')(1).replace("concept=", "").trim)
        if (alignments.getConceptAlignments(con).map(_.toString.replace("http://", "").replace("https://", "")).contains(uri)) {
          return Server.TextResponse("URI " + uri + " already aligned with \"" + con + "\"!")
        }
        val ref = Try(LogicalReference(Path.parseMS(uri, NamespaceMap.empty))).getOrElse(PhysicalReference(URI(uri)))
        val alig = ConceptAlignment(ref, con)
        alignments.addNew(alig)
        if (!conlist.contains(con)) {
          conlist = alignments.getConcepts
          menu = HTML.build(makeMenu)
          // saveIndex(con.head.toLower)
        }
        log("Added URI " + ref + " to concept: " + con)
        Server.TextResponse("Added URI " + ref + " to concept: " + con + "\nTHANK YOU FOR CONTRIBUTING!")
      }
    } else if (path == List("addFormal") && query != "") {
      println(query)
      val qs = query.split("&")
      val nsm = NamespaceMap.empty
      val from = qs.find(_.startsWith("from=")).getOrElse(???).drop(5)
      val to = qs.find(_.startsWith("to=")).getOrElse(???).drop(3)
      if (from == to) Server.errorResponse("Alignments must be between two different URIs!") else {
        val invertible = qs.exists(_.startsWith("invertible="))
        val parstring = qs.find(_.startsWith("attributes=")).map(s => URLEscaping.unapply(s.drop(11)).trim)
        var rest = parstring.getOrElse("")
        val param = """(.+)\s*=\s*\"(.+)\"\s*(.*)""".r
        var pars : List[(String,String)] = List(("direction",if (invertible) "both" else "forward"))
        while (rest != "") rest match {
          case param(key, value, r) ⇒
            pars ::= (key, value)
            rest = r.trim
          case _ ⇒ Server.errorResponse("Malformed alignment: " + rest)
        }
        val al = alignments.makeAlignment(from,to,pars)
        alignments.addNew(al)
        Server.TextResponse("New Alignment added: " + al.toString + "\nThank you!")
      }
    } else if (path.isEmpty && query == "conlist") {
      log("Query for conlist")
      Server.TextResponse("[" + conlist.map(s => "\"" + s + "\"").mkString(",") + "]")
    } else if (path.isEmpty && query.startsWith("page=")) {
      val index = query(5).toLower
      log("Query for page " + index)
      Server.TypedTextResponse(doFullPage(List(index.toString)),"html")
    } else if (path.isEmpty && query.startsWith("con=")) {
      val con = URLEscaping.unapply(query.drop(4))
      log("CALL constructing concept " + con)
      Server.TypedTextResponse(doFullPage(List("con",con)),"html")
    } else Server.TypedTextResponse(doFullPage(List("a")),"html")

  lazy val presenter = controller.extman.get(classOf[Presenter],"html").get.asInstanceOf[HTMLPresenter]

  def makeConceptPage(con : String, h : HTML) = {
    val refs = alignments.getConceptAlignments(con).map(_.to)
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
    }).distinct.map(s =>
      if (s.contains("wikipedia")) (s,0)
      else if (s.contains("mathworld.wolfram")) (s,1)
      else if (s.contains("encyclopediaofmath.org")) (s,2)
      else if (s.contains("ncatlab.org")) (s,3)
      else if (s.contains("planetmath.org")) (s,4)
      else (s,5)
    ).sortBy(_._2).map(_._1)
    val formalaligs = formals.flatMap(p => alignments.getFormalAlignments(p)).filterNot(_.isGenerated)
    import h._
    implicit val rh = new HTMLRenderingHandler(h)
      span {
        h1() {text{con}}
        if (altnames.nonEmpty) div {
          text {"(See also: "}
          altnames.init.foreach(n => {
            a(":concepts?con=" + URLEscaping(n)) {text {n} }
            text {", "}
          })
          a(":concepts?con=" + URLEscaping(altnames.last)) {text {altnames.last} }
          text {")"}
        }
        def doInformal(s : => Unit) = tr { td(cls="ext-table",attributes = List(("align","center"))) { s }}
        if (informals.nonEmpty) p {
          h3 { text { "External Resources"} }
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
          h3 { text{ "Formal Libraries" } }
          div(cls="document toggle-root inlineBoxSibling",id="formal") {
          table(id="formal-table",cls="formal-table",attributes = List(("width","90%"))) {
            val pairs = formals map (path => {
              val src = controller.getO(path)
              val arch = if (src.isDefined) controller.backend.findOwningArchive(path match {
                case mp : MPath => mp
                case gn : GlobalName => gn.module
              }).map(_.id) else None
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
          // TODO pagebottom
          if (formalaligs.nonEmpty) doAlignments(h,formalaligs)
          val allformals = /* (formals ::: altnames.flatMap(alignments.getConceptAlignments).map(_.to).collect{
            case LogicalReference(path) => path
          }).distinct */ alignments.getAlignments(con).flatMap(al => List(al.from,al.to)) collect {
            case LogicalReference(path) => path
          }
          if (allformals.length > 1) doAlignmentCommit(h,allformals)
        }

      }
  }
  private def doAlignmentCommit(h : HTML,formals : List[ContentPath]): Unit = {
    import h._
    h3 {text {"Add Formal Alignment"}}
    form(":concepts/addFormal") { table {
      tr {
        td {text {"From: "}}
        td {select("from") {
          formals foreach (f => option(f.toString) { text {f.toString}})
        }}
      }
      tr {
        td { text {"To: "} }
        td {select("to") {
          formals foreach (f => option(f.toString) { text {f.toString}})
        }}
      }
      tr {
        td {}
        td {input("checkbox", name = "invertible", value = "invertible") {
          text { "invertible" }
        }}
      }
      tr {
        td { text { "Attributes:" } }
        td { input("text", name = "attributes", value = "arguments=\"(1,2)\"") {  } }
      }
      tr {
        td { input("submit",value="Submit") {} }
        td {}
      }
    } }
  }
  private def doAlignments(h : HTML,aligs : List[FormalAlignment]): Unit = {
    import h._
    h3 {
      text { "Formal Alignments" }
    }
    table(cls="formal-table",attributes = List(("width","90%"))) {
      tr {
        th {text { "From" }}
        th {text { "To" }}
        th {text { "Arguments" }}
        th {text { "Invertible" }}
        th {text { "Properties" }}
      }
      aligs foreach (al => tr {
        td { a("./?" + al.from.mmturi.toString) {text {al.from.mmturi.toString} } }
        td { a("./?" + al.to.mmturi.toString) {text {al.to.mmturi.toString} } }
        td {text {al match {
          case ArgumentAlignment(_,_,_,args) => args.map(p => p._1 + "=>" + p._2).mkString(", ")
          case _ => "Simple"
        }}}
        td {text { if (al.invertible) "yes" else "no"}}
        td {text { al.props.map(p => p._1 + "=" + p._2).mkString(", ") }}
      })
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
                         <title>MMT Web Server</title>
                         <script type="text/javascript" src="script/jquery/jquery.js"></script>
                         <script type="text/javascript" src="script/jquery/jquery-ui.js"></script>
                         <link rel="stylesheet" type="text/css" href="css/bootstrap/css/bootstrap.min.css"></link>
                         <link rel="stylesheet" type="text/css" href="css/mmt.css" />
                         <link rel="stylesheet" type="text/css" href="css/concepts.css" />
                         <link rel="stylesheet" type="text/css" href="css/JOBAD.css" />
                         <link rel="stylesheet" type="text/css" href="css/jquery/jquery-ui.css" />
                         <link rel="shortcut icon" href="mmt2.png" />


                   <!-- Core JS API for MMT interaction -->
                         <script type="text/javascript" src="script/mmt/mmt-js-api.js"></script>
                   <!-- JOBAD Deps -->
                        <script type="text/javascript" src="script/jobad/deps/underscore-min.js"></script>
                   <!-- JOBAD -->
                         <script type="text/javascript" src="script/jobad/JOBAD.js"></script>
                   <!-- JOBAD Services -->
                         <script type="text/javascript" src="script/jobad/modules/hovering.js"></script>
                         <script type="text/javascript" src="script/jobad/modules/interactive-concepts.js"></script>
                         <script type="text/javascript" src="script/mmt/concepts.js"></script>"""
}
