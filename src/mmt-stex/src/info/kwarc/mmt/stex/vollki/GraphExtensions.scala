package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.{DPath, MPath, NamespaceMap, Path, StructuralElement}
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.ontology.{Declares, IsDocument, IsTheory}
import info.kwarc.mmt.api.symbols.DerivedDeclaration
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONObject}
import info.kwarc.mmt.api.web.{GraphSolverExtension, JGraphBuilder, JGraphEdge, JGraphExporter, JGraphNode, JGraphSelector, ServerExtension, StandardBuilder}
import info.kwarc.mmt.stex.{STeX, STeXServer}

import scala.collection.{Set, mutable}

class STeXGraphPopulator extends Extension {
  override def start(args: List[String]): Unit = {
    if (controller.extman.get(classOf[STeXServer]).isEmpty) controller.extman.addExtension("info.kwarc.mmt.stex.STeXServer",Nil)
    if (!controller.extman.get(classOf[Extension]).contains(FullsTeXGraph)) controller.extman.addExtension(FullsTeXGraph)
    FullsTeXGraph.populate()
    controller.extman.removeExtension(this)
  }
}

object FullsTeXGraph extends Extension {

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

  trait sTeXNode {
    val id : String
    protected def selectTopo : List[sTeXNode]
    lazy val topologicalSort : List[sTeXNode] = {
      (selectTopo.flatMap(_.topologicalSort) ::: List(this)).distinct
    }
  }
  class sTeXDocument(val id : String) extends sTeXNode {
    private[FullsTeXGraph] var _children : List[sTeXNode] = Nil
    def children = _children
    private[FullsTeXGraph] var _languages : List[String] = Nil
    def languages = _languages
    private[FullsTeXGraph] val _documents = mutable.HashMap.empty[String,Document]
    def getDocument(language : String) : Option[Document] = _documents.get(language)

    override protected def selectTopo: List[sTeXNode] = _children
  }
  class sTeXTheory(val sig: Theory) extends sTeXNode {
    val path : MPath = sig.path
    val id = path.toString
    private[FullsTeXGraph] var _includes : List[sTeXTheory] = Nil
    def includes = _includes
    private[FullsTeXGraph] var _uses : List[sTeXTheory] = Nil
    def uses = _uses
    private[FullsTeXGraph] var _languages : List[String] = Nil
    def languages = _languages
    def getLanguageModule(language : String) : Option[Theory] = controller.getAsO(classOf[Theory],path.toDPath ? language)
    private[FullsTeXGraph] val _documents = mutable.HashMap.empty[String,Document]
    def getDocument(language : String) : Option[Document] = _documents.get(language)

    override protected def selectTopo: List[sTeXNode] = _includes
  }

  val all_languages = List("en","de","ar","bg","ru","fi","ro","tr","fr")

  private var allobjects : List[sTeXNode] = Nil
  def getAll() = allobjects
  private val objects = mutable.HashMap.empty[Path,sTeXNode]
  private def doDoc(d : Document): Option[sTeXDocument] = {
    var pathstr = d.path.toString
    if (!pathstr.endsWith(".omdoc")) {
      return None
    }
    pathstr = pathstr.dropRight(6)
    val (id,lang) = if (all_languages.exists(s => pathstr.endsWith("." + s))) {
      val ls = pathstr.split('.')
      (pathstr.init.mkString("."),pathstr.last)
    } else (pathstr,"")
    val node = new sTeXDocument(id)
    allobjects ::= node
    objects(Path.parse(id)) = node
    if (lang == "") {
      node._documents("") = d
      objects(d.path) = node
      d.getDeclarations.foreach {
        case mr : MRef =>
          getO(mr.target).foreach(node._children ::= _)
        case dr : DRef =>
          getO(dr.target).foreach(node._children ::= _)
        case _ =>
      }
    } else {
      all_languages.foreach {l =>
        controller.getO(Path.parseD(id + "." + l + ".omdoc",NamespaceMap.empty)) match {
          case Some(d : Document) =>
            objects(d.path) = node
            node._documents(l) = d
            node._languages ::= l
            d.getDeclarations.foreach {
              case mr : MRef =>
                getO(mr.target).foreach(node._children ::= _)
              case dr : DRef =>
                getO(dr.target).foreach(node._children ::= _)
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
    val (id,lang) = if (all_languages.exists(s => pathstr.endsWith("." + s))) {
      val ls = pathstr.split('.')
      (pathstr.init.mkString("."),pathstr.last)
    } else (pathstr,"")
    val node = new sTeXTheory(th)
    allobjects ::= node
    objects(Path.parse(id)) = node
    objects(th.path) = node
    if (lang == "") {
      node._documents("") = d
      objects(d.path) = node
    } else {
      all_languages.foreach {l =>
        controller.getO(Path.parseD(id + "." + l + ".omdoc",NamespaceMap.empty)) match {
          case Some(d : Document) =>
            objects(d.path) = node
            node._documents(l) = d
            node._languages ::= l
          case _ =>
        }
      }
    }
    doTheory(th,node)
    Some(node)
  }

  private def doTheory(th: Theory,node:sTeXTheory): Unit = {
    all_languages.foreach { s =>
      node.getLanguageModule(s) match {
        case Some(t: Theory) =>
          node._languages ::= s
          objects(t.path) = node
          t.getAllIncludesWithoutMeta.flatMap(i => getO(i.from)).filter(_ != node).foreach { case ch : sTeXTheory =>
            node._uses ::= ch
          }
        case _ =>
      }
    }
    node._uses = node._uses.distinct
    th.getAllIncludesWithoutMeta.flatMap(i => getO(i.from)).foreach{ case ch : sTeXTheory =>
      node._includes ::= ch
    }
  }

  private def checkDoc(d : Document) : Option[Theory] = {
    d.getDeclarations match {
      case List(a : MRef) =>
        controller.getO(a.target) match {
          case Some(th : Theory) =>
            th.metadata.get(STeX.meta_language) match {
              case List(_) if all_languages.contains(th.name.toString) =>
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
              case List(_) if all_languages.contains(tha.name.toString) && thb.path == tha.path.parent.toMPath =>
                Some(thb)
              case _ =>
                thb.metadata.get(STeX.meta_language) match {
                  case List(_) if all_languages.contains(thb.name.toString) && tha.path == thb.path.parent.toMPath =>
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
                      val st = new sTeXTheory(th)
                      allobjects ::= st
                      objects(p) = st
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
                  ???
              }
            case List(_) if all_languages.contains(th.name.toString) =>
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
          override val uri: Option[String] = Some(th.sig.path.toString)
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
      ("nodes",JSONArray(jnodes.map(_.toJSON) :_*)),
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
    if (ret.length == 1) ret.head.topologicalSort else ret
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
