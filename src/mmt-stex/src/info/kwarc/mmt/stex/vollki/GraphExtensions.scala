package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.{DPath, MPath, NamespaceMap, Path, StructuralElement}
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMFOREIGN
import info.kwarc.mmt.api.ontology.{Declares, IsDocument, IsTheory}
import info.kwarc.mmt.api.symbols.DerivedDeclaration
import info.kwarc.mmt.api.utils.{File, JSON, JSONArray, JSONObject, JSONString, MMTSystem}
import info.kwarc.mmt.api.web.{GraphSolverExtension, JGraphBuilder, JGraphEdge, JGraphExporter, JGraphNode, JGraphSelector, ServerExtension, ServerRequest, ServerResponse, StandardBuilder}
import info.kwarc.mmt.stex.Extensions.DocumentExtension
import info.kwarc.mmt.stex.xhtml.HTMLParser
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, empty}
import info.kwarc.mmt.stex.{STeX, STeXServer}

import scala.collection.{Set, mutable}
import scala.xml.{Elem, Node, XML}

class STeXGraphPopulator extends Extension {
  override def start(args: List[String]): Unit = {
    if (controller.extman.get(classOf[STeXServer]).isEmpty) controller.extman.addExtension("info.kwarc.mmt.stex.STeXServer",Nil)
    if (!controller.extman.get(classOf[Extension]).contains(FullsTeXGraph)) controller.extman.addExtension(FullsTeXGraph)
    FullsTeXGraph.populate()
    controller.extman.removeExtension(this)
  }
}

object FullsTeXGraph extends ServerExtension("vollki") {

  lazy val server = controller.extman.get(classOf[STeXServer]).head

  def usermodels = controller.extman.get(classOf[UserModels]).headOption

  override def apply(request: ServerRequest): ServerResponse = {
    val path = request.parsedQuery("path").flatMap(s => getO(Path.parse(s)))
    val user = request.parsedQuery("user").flatMap(u => usermodels.flatMap(_.getUser(u))) match {
      case None => usermodels.flatMap(_.getUser("nulluser"))
      case Some(u) => Some(u)
    }
    val language = request.parsedQuery("lang") match {
      case Some(l) => l
      case None => "en"
    }

    request.path.lastOption match {
      case Some(":vollki") =>
          var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
          html = html.replace("TOUR_ID_PLACEHOLDER",path.map(_.id).getOrElse(""))
          html = html.replace("BASE_URL_PLACEHOLDER","")
        html = html.replace("CONTENT_URL_PLACEHOLDER","")
          html = html.replace("USER_MODEL_PLACEHOLDER",user.map(_.f.name).getOrElse(""))
          html = html.replace("LANGUAGE_PLACEHOLDER",language)
          ServerResponse(html, "text/html")
      case Some("list") =>
        ServerResponse.JsonResponse(JSONArray(
          getAll().map(e => JSONObject(("value",JSONString(e.id)),("label",JSONString(e.getTitle(language).toString())))) :_*
        ))
      case Some("frag") =>
        path.foreach {node =>
          val doc = node.getDocument(language).getOrElse(node.getDocument("en").getOrElse(node.getDocument("").get))
          val file = controller.backend.resolveLogical(doc.path.uri).map { case (a, ls) =>
            (File(a.rootString) / "xhtml" / ls.mkString("/")).setExtension("xhtml")
          }.getOrElse {
            return ServerResponse("Document not found: " + doc.path, "text/plain")
          }
          val html = HTMLParser.apply(File.read(file))(server.getState)
          val exts = server.extensions
          val docrules = exts.collect {
            case e: DocumentExtension =>
              e.documentRules
          }.flatten
          def doE(e: HTMLNode): Unit = docrules.foreach(r => r.unapply(e))
          html.iterate(doE)
          val dbody = html.get("body")()().head
          return ServerResponse(dbody.children.map(_.toString).mkString("\n"), "application/xhtml+xml")
        }
        ServerResponse("Unknown query: " + request.query, "text/plain")
      case Some("tour") =>
        path match {
          case Some(n) =>
            val node = n.topologicalSort.filter(c => user.forall(u => u.getValue(c) < 0.9))
            ServerResponse.JsonResponse(node.jsonify(language))
          case None =>
            ServerResponse("Unknown query: " + request.query, "text/plain")
        }
      case _ =>
        ServerResponse("Unknown request: " + request.path.mkString("/"),"text/plain")
    }
  }

  override def start(args: List[String]): Unit = {
    if (!controller.extman.get(classOf[JGraphExporter]).contains(STeXGraph)) controller.extman.addExtension(STeXGraph)
  }

  private var is_populated : Boolean = false

  def populate(): Unit = if (!is_populated) {
    is_populated = true
    println("Populating sTeX Theory Graph...")
    val docs = controller.backend.getArchives.filter{a =>
      a.properties.get("format").contains("stex")
    }.flatMap {a =>
      val b = a.narrationBase
      controller.depstore.getInds(IsDocument).toList.filter(_.toString.startsWith(b.toString))
    }
    val len = docs.length
    docs.zipWithIndex.foreach { case (p,i) =>
      print( "\r" + (i+1) + "/" + len + "...")
      getO(p)
    }
    println("Done. Found " + allobjects.length + " nodes")
    val olen = allobjects.length
    println("Topological sorting...")
    allobjects.zipWithIndex.foreach { case (o,i) =>
      print( "\r" + (i+1) + "/" + olen + "...")
      o.topologicalSort
    }
    println("Done.")
  }

  class MutableList[A] {
    var dones: List[A] = Nil
    var count : Int = 0
  }
  case class TopoDependencies(parent : sTeXNode,children:List[TopoDependencies]) {
    def filter(f: sTeXNode => Boolean): TopoDependencies = TopoDependencies(parent,
      children.filter(c => f(c.parent)).map(_.filter(f))
    )

    def jsonify(language:String):JSONArray = {
      val map = mutable.HashMap.empty[sTeXNode,MutableList[sTeXNode]]
      jsonifyI(map)
      JSONArray(map.toList.sortBy(_._2.count).map(p => JSONObject(
        ("id",JSONString(p._1.id)),
        ("title",JSONString(p._1.getTitle(language).toString())),
        ("successors",JSONArray(p._2.dones.map(c => JSONString(c.id)):_*))
      )):_*)
    }
    protected def jsonifyI(map : mutable.HashMap[sTeXNode,MutableList[sTeXNode]]): Unit = if (!map.contains(this.parent)) {
      val ls = new MutableList[sTeXNode]
      ls.count = children.length
      map(this.parent) = ls
      this.children.foreach{c =>
        c.jsonifyI(map)
        val cc = map(c.parent)
        cc.dones ::= this.parent
        ls.count += cc.count
      }
    }
    def nodes(o : MutableList[sTeXNode] = new MutableList[sTeXNode]) : List[sTeXNode] = if (o.dones.contains(this.parent)) Nil else {
      o.dones ::= this.parent
      this.parent :: children.flatMap(_.nodes(o))
    }
  }

  trait sTeXNode {
    val id : String
    def selectTopo : List[sTeXNode]
    private var _top : Option[TopoDependencies] = None
    def topologicalSort : TopoDependencies = _top.getOrElse {
      val ret = TopoDependencies(this,selectTopo.map(_.topologicalSort))
      //val ret = (selectTopo.flatMap(_.topologicalSort) ::: List(this)).distinct
      _top = Some(ret)
      ret
    }
    private def getDocumentDefinitely(language : String) = getDocument(language) match {
      case Some(d) => Some(d)
      case _ => getDocument("en") match {
        case Some(d) => Some(d)
        case _ => getDocument("")
      }
    }
    def getTitle(language: String) : Node = getDocumentDefinitely(language).flatMap(
      _.metadata.get(STeX.meta_doctitle).headOption.map(_.value match {
        case OMFOREIGN(node) => node
        case _ => <span>{id}</span>
      })
    ).getOrElse(<span>{id}</span>)
    val parents = mutable.HashSet.empty[sTeXNode]
    val keys = mutable.HashSet.empty[Path]
    protected[FullsTeXGraph] val _documents = mutable.HashMap.empty[String,DPath]
    def getDocument(language : String) : Option[Document] = _documents.get(language).flatMap(controller.getAsO(classOf[Document],_))
  }
  class sTeXDocument(val id : String) extends sTeXNode {
    private[FullsTeXGraph] var _children : List[sTeXNode] = Nil
    def children = _children
    private[FullsTeXGraph] var _languages : List[String] = Nil
    def languages = _languages

    override def selectTopo: List[sTeXNode] = _children.reverse
  }
  class sTeXTheory(val sig: MPath) extends sTeXNode {
    val path : MPath = sig
    val id = path.toString
    private[FullsTeXGraph] var _includes : List[sTeXTheory] = Nil
    def includes = _includes
    private[FullsTeXGraph] var _uses : List[sTeXTheory] = Nil
    def uses = _uses
    private[FullsTeXGraph] var _languages : List[String] = Nil
    def languages = _languages
    def getLanguageModule(language : String) : Option[Theory] = controller.getAsO(classOf[Theory],path.toDPath ? language)

    override def selectTopo: List[sTeXNode] = _includes.reverse
  }

  private var allobjects : List[sTeXNode] = Nil
  def getAll() = allobjects
  private val objects = mutable.HashMap.empty[Path,sTeXNode]
  private def doDoc(d : Document): Option[sTeXDocument] = {
    var pathstr = d.path.toString
    if (!pathstr.endsWith(".omdoc")) {
      return None
    }
    pathstr = pathstr.dropRight(6)
    val (id,lang) = if (STeX.all_languages.exists(s => pathstr.endsWith("." + s))) {
      val ls = pathstr.split('.')
      (ls.init.mkString("."),ls.last)
    } else (pathstr,"")
    val node = new sTeXDocument(id)
    allobjects ::= node
    objects(Path.parse(id)) = node
    node.keys.add(Path.parse(id))
    if (lang == "") {
      node._documents("") = d.path
      objects(d.path) = node
      node.keys.add(d.path)
      d.getDeclarations.foreach {
        case mr : MRef =>
          getO(mr.target).foreach{c => node._children ::= c; c.parents.add(node)}
        case dr : DRef =>
          getO(dr.target).foreach{c => node._children ::= c; c.parents.add(node)}
        case _ =>
      }
    } else {
      STeX.all_languages.foreach {l =>
        controller.getO(Path.parseD(id + "." + l + ".omdoc",NamespaceMap.empty)) match {
          case Some(d : Document) =>
            objects(d.path) = node
            node.keys.add(d.path)
            node._documents(l) = d.path
            node._languages ::= l
            d.getDeclarations.foreach {
              case mr : MRef =>
                getO(mr.target).foreach{c => node._children ::= c; c.parents.add(node)}
              case dr : DRef =>
                getO(dr.target).foreach{c => node._children ::= c; c.parents.add(node)}
              case _ =>
            }
          case _ =>
        }
      }
    }
    Some(node)
  }

  private def doThDoc(th : Theory,d : Document): Option[sTeXTheory] = {
    var pathstr = d.path.toString
    if (!pathstr.endsWith(".omdoc")) {
      return None
    }
    pathstr = pathstr.dropRight(6)
    val (id,lang) = if (STeX.all_languages.exists(s => pathstr.endsWith("." + s))) {
      val ls = pathstr.split('.')
      (ls.init.mkString("."),ls.last)
    } else (pathstr,"")
    val node = new sTeXTheory(th.path)
    allobjects ::= node
    objects(Path.parse(id)) = node
    objects(th.path) = node
    node.keys.add(Path.parse(id))
    node.keys.add(th.path)
    if (lang == "") {
      node._documents("") = d.path
      objects(d.path) = node
      node.keys.add(d.path)
    } else {
      STeX.all_languages.foreach {l =>
        controller.getO(Path.parseD(id + "." + l + ".omdoc",NamespaceMap.empty)) match {
          case Some(d : Document) =>
            objects(d.path) = node
            node.keys.add(d.path)
            node._documents(l) = d.path
            node._languages ::= l
          case _ =>
        }
      }
    }
    doTheory(th,node)
    Some(node)
  }

  private def doTheory(th: Theory,node:sTeXTheory): Unit = {
    STeX.all_languages.foreach { s =>
      node.getLanguageModule(s) match {
        case Some(t: Theory) =>
          node._languages ::= s
          objects(t.path) = node
          node.keys.add(t.path)
          t.getAllIncludesWithoutMeta.flatMap(i => getO(i.from)).filter(_ != node).foreach { case ch : sTeXTheory =>
            node._uses ::= ch
          }
        case _ =>
      }
    }
    node._uses = node._uses.distinct
    th.getAllIncludesWithoutMeta.flatMap(i => getO(i.from)).foreach{ case ch : sTeXTheory =>
      node._includes ::= ch
      ch.parents.add(node)
    }
  }

  private def checkDoc(d : Document) : Option[Theory] = {
    d.getDeclarations match {
      case List(a : MRef) =>
        controller.getO(a.target) match {
          case Some(th : Theory) =>
            th.metadata.get(STeX.meta_language) match {
              case List(_) if STeX.all_languages.contains(th.name.toString) =>
                controller.getO(th.path.parent.toMPath) match {
                  case Some(th : Theory) =>
                    Some(th)
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      case List(a:MRef,b:MRef) =>
        (controller.getO(a.target),controller.getO(b.target)) match {
          case (Some(tha : Theory),Some(thb : Theory)) =>
            tha.metadata.get(STeX.meta_language) match {
              case List(_) if STeX.all_languages.contains(tha.name.toString) && thb.path == tha.path.parent.toMPath =>
                Some(thb)
              case _ =>
                thb.metadata.get(STeX.meta_language) match {
                  case List(_) if STeX.all_languages.contains(thb.name.toString) && tha.path == thb.path.parent.toMPath =>
                    Some(tha)
                  case _ => None
                }
            }
          case _ => None
        }
      case _ => None
    }
  }

  def getO(p : Path) : Option[sTeXNode] = objects.get(p) match {
    case Some(o) => Some(o)
    case None =>
      controller.getO(p) match {
        case None => None
        case Some(_:DerivedDeclaration) => None
        case Some(th : Theory) =>
          th.metadata.get(STeX.meta_language) match {
            case Nil =>
              controller.depstore.querySet(th.path,-Declares).toList match {
                case List(a) =>
                  controller.getO(a) match {
                  case Some(d: Document) => checkDoc(d) match {
                    case Some(_) => doThDoc(th, d)
                    case _ =>
                      val st = new sTeXTheory(th.path)
                      allobjects ::= st
                      objects(p) = st
                      st.keys.add(p)
                      getO(d.path).foreach {
                        case doc: sTeXDocument =>
                          doc._documents.foreach { case (k, v) => st._documents(k) = v }
                      }
                      doTheory(th, st)
                      Some(st)
                  }
                  case _ =>
                    None
                }
                case _ =>
                  None
              }
            case List(_) if STeX.all_languages.contains(th.name.toString) =>
              getO(th.path.parent.toMPath)
            case _ =>
              ???
          }
        case Some(d : Document) =>checkDoc(d) match { // <- doesn't work
          case Some(th) => doThDoc(th, d)
          case _ =>
            doDoc(d)
        }
      }
  }

}

object STeXGraph extends JGraphExporter("stexgraph") {
  override def buildGraph(s: String): JSON = {
    val nodes = s.split(',').map(_.trim).flatMap(select).distinct
    var edges : List[JGraphEdge] = Nil
    val jnodes = nodes.map {
      case th : FullsTeXGraph.sTeXTheory =>
        th.includes.foreach{n =>
          edges ::= new JGraphEdge {
            override val id: String = th.id + "/[" + n.id + "]"
            override val style: String = "include"
            override val from: String = n.id
            override val to: String = th.id
            override val label: Option[String] = None
            override val uri: Option[String] = None
          }
        }
        th.uses.foreach{n =>
          edges ::= new JGraphEdge {
            override val id: String = th.id + "/[" + n.id + "]"
            override val style: String = "structure"
            override val from: String = n.id
            override val to: String = th.id
            override val label: Option[String] = None
            override val uri: Option[String] = None
          }
        }
        new JGraphNode {
          override val id: String = th.id
          override val style: String = "theory"
          override val label: Option[String] = Some(th.sig.name.toString)
          override val uri: Option[String] = Some(th.sig.toString)
        }
      case doc : FullsTeXGraph.sTeXDocument =>
        doc.children.foreach{n =>
          edges ::= new JGraphEdge {
            override val id: String = doc.id + "/[" + n.id + "]"
            override val style: String = "view"
            override val from: String = n.id
            override val to: String = doc.id
            override val label: Option[String] = None
            override val uri: Option[String] = None
          }
        }
        new JGraphNode {
          val id = doc.id
          val style = "model"
          val label = Some(doc.id.split('/').last)
          val uri = Some(doc.id)
        }
    }
    JSONObject(
      ("nodes",JSONArray(jnodes.map(_.toJSON).toIndexedSeq :_*)),
      ("edges",JSONArray(edges.map(_.toJSON) :_*))
    )
  }

  def select(s : String) : List[FullsTeXGraph.sTeXNode] = {
    val ret = if (s == "full") {
      FullsTeXGraph.populate()
      FullsTeXGraph.getAll()
    } else {
      val inds = (controller.depstore.getInds(IsDocument).toList :::
        controller.depstore.getInds(IsTheory).toList)
      val archive_candidates = controller.backend.getArchives.filter{a =>
        a.id.startsWith(s) && a.properties.get("format").contains("stex")
      }
      if (archive_candidates.nonEmpty) {
        inds.filter(i => archive_candidates.exists{ a =>
          i.toString.startsWith(a.narrationBase.toString)
        }).flatMap(FullsTeXGraph.getO)
      } else {
        inds.filter(_.toString.startsWith(s)).flatMap(FullsTeXGraph.getO)
      }
    }
    if (ret.length == 1) ret.head.topologicalSort.nodes() else ret
  }

  def computeSem(f: JSON, sem: String, comp: String = "default"): JSON = {
    val key = "default"
    controller.extman.get(classOf[GraphSolverExtension]).find(_.key == key) match {
      case Some(e) => e(f, sem, comp)
      case None => log("No solver present")
        f
    }
  }

}
