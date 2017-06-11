package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension, FormatBasedExtension}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.ontology.Declares._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation.StructurePresenter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.util.Try

/**
  * Created by jazzpirate on 07.06.17.
  */
class JSONBasedGraphServer extends ServerExtension("jgraph") {
  override val logPrefix = "jgraph"
  private case class CatchError(s : String) extends Throwable

  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(new JGraphSideBar)
    controller.extman.addExtension(new JDocgraph)
    controller.extman.addExtension(new JThgraph)
    controller.extman.addExtension(new JPgraph)
    controller.extman.addExtension(new JArchiveGraph)
    super.start(args)
  }

  lazy val sidebar = controller.extman.get(classOf[JGraphSideBar]).head


  def apply(request: ServerRequest): ServerResponse = {
    log("Paths: " + request.extensionPathComponents)
    log("Query: " + request.queryString)
    log("Path: " + request.parsedQuery("uri"))
    if (request.extensionPathComponents.headOption == Some("menu")) {
      val id = request.parsedQuery("id").getOrElse("top")
      log("Returing menu for " + id)
      if (id == "full") ServerResponse.fromJSON(sidebar.getJSON("top",true))
      else ServerResponse.fromJSON(sidebar.getJSON(id))
    } else if (request.extensionPathComponents.headOption == Some("json")) {
      val uri = request.parsedQuery("uri").getOrElse(return ServerResponse.plainErrorResponse(GetError("Not a URI")))
      val key = request.parsedQuery("key").getOrElse("pgraph")
      val exp = controller.extman.getOrAddExtension(classOf[JGraphExporter], key).getOrElse {
        throw CatchError(s"exporter $key not available")
      }
      log("Returning " + key + " for " + uri)
      ServerResponse.fromJSON(exp.buildGraph(uri))
    } else ServerResponse.errorResponse("Invalid path",request)
  }
}

class JGraphSideBar extends Extension {
  private class Tree(val id : String, val str : String, val uri : String, val tp : String) {
    var children : List[Tree] = Nil
    def add(ch : Tree) = if (!children.contains(ch)) {
      children ::= ch
    }
    def toJSON : JSON = JSONObject(
      ("menuText",JSONString(str)),
      ("id",JSONString(id)),
      ("uri",JSONString(uri)),
      ("type",JSONString(tp)),
      ("children",JSONArray(children.map(c => (c.str,c.id)).sortBy(_._1).distinct.map(p =>
        JSONObject(("menuText",JSONString(p._1)),("id",JSONString(p._2)))):_*)))
    def fullJSON : JSON = JSONObject(
      ("menuText",JSONString(str)),
      ("id",JSONString(id)),
      ("uri",JSONString(uri)),
      ("type",JSONString(tp)),
      ("children",JSONArray(children.sortBy(_.str).map(_.fullJSON).distinct:_*)))
  }
  private object Tree {
    private val hm = scala.collection.mutable.HashMap.empty[String,Tree]
    def apply(id : String, str : String = "", uri : String = "", tp : String ="pathgraph") = {
      hm.getOrElse(id, {
        val ret = new Tree(id, if (str=="") id else str, uri, tp)
        hm(id) = ret
        ret
      })
    }
  }

  def getJSON(id : String, full : Boolean = false) = if (id == "top") JSONArray(archs.map(doArchive).sortBy(_.str).distinct.map(t =>
    if (full) t.fullJSON else t.toJSON):_*)
    else if (full) Tree(id).fullJSON else Tree(id).toJSON

  private def trimpath(p : Path) = if (p.toString.trim.last == '/') p.toString.trim.init else p.toString.trim

  private def doArchive(a : Archive) : Tree = {
    val ret = if (a.id contains "/") {
      val List(pre,post) = utils.stringToList(a.id,"/")
      Tree(pre,pre,pre,"archivegraph").add(Tree(a.id,post,a.id,"archivegraph"))
      Tree(pre)
    } else Tree(a.id,a.id,a.id,"archivegraph")
    Tree(a.id).add(Tree(a.id + "-narr","Narration",a.narrationBase.toString,"thgraph"))
    Tree(a.id).add(Tree(a.id + "-cont","Content",a.id,"archivegraph"))
    doNarr(DPath(a.narrationBase)).children.foreach(Tree(a.id + "-narr").add)
    val mods = a.allContent()
    mods.map(p => doCont(p,a.id)).distinct.foreach(Tree(a.id + "-cont").add)
    ret
  }
  private def doCont(p : Path,suffix : String = "") : Tree = p match {
    case mp : MPath =>
      val ret = doCont(mp.parent,suffix)
      Tree(trimpath(mp.parent) + "-cont" + "-" + suffix).add(Tree(trimpath(mp) + "-cont" + "-" + suffix,"?" + mp.name.toString,trimpath(mp),"thgraph"))
      ret
    case dp : DPath =>
      if (dp.ancestors.length == 1) Tree(trimpath(dp) + "-cont" + "-" + suffix,trimpath(dp),"pgraph")
      else {
        val ret = doCont(dp.^^)
        ret.add(Tree(trimpath(dp) + "-cont" + "-" + suffix,dp.last,trimpath(dp),"pgraph"))
        ret
      }
  }
  private def doNarr(p : Path) : Tree = p match {
    case dp: DPath =>
      val doc = Try(controller.getDocument(dp)).toOption
      val s2 = doc match {
        case Some(d) if !d.root => d.path.last
        case _ => trimpath(dp)
      }
      doc.foreach(_.getDeclarations.foreach {
        case r: NRef => Tree(trimpath(dp) + "-narr",s2,trimpath(dp),"docgraph").add(doNarr(r.target))
        case doc: Document => Tree(trimpath(dp) + "-narr",s2,trimpath(dp),"docgraph").add(doNarr(doc.path))
        case _ =>
      })
      Tree(trimpath(dp) + "-narr",s2,trimpath(dp),"docgraph")
    case mp: MPath =>
      Tree(trimpath(mp) + "-narr","?" + mp.name.toString,trimpath(mp),"thgraph")
  }

  lazy private val archs = controller.backend.getArchives.sortBy(_.id)
  // lazy private val top = archs.map(doArchive).sortBy(_._1).distinct.map(p => (p._1,p._2.toJSON))

}

abstract class JGraphExporter(val key : String) extends FormatBasedExtension {
  def isApplicable (format: String): Boolean = format == key
  def buildGraph(s : String) : JSON
}

abstract class SimpleJGraphExporter(key : String)
  extends JGraphExporter(key) {

  val builder : JGraphBuilder
  val selector : JGraphSelector

  def buildGraph(s : String) : JSON = {
    val (ths,vs) = selector.select(s)(controller)
    builder.build(ths,vs)(controller)
  }

}

class JDocgraph extends SimpleJGraphExporter("docgraph"){
  val builder = GraphBuilder.PlainBuilder(false)
  val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[DeclaredTheory], List[View]) = {
      val se = Try(controller.get(Path.parse(s))) match {
        case scala.util.Success(e : StructuralElement) => e
        case _ => return (Nil,Nil)
      }
      val (theories, views) = se match {
        case doc: Document =>
          (controller.depstore.querySet(doc.path, Transitive(+Declares) * HasType(IsTheory)),
            controller.depstore.querySet(doc.path, Transitive(+Declares) * HasType(IsView))
            )
        case thy: Theory => (List(thy.path), Nil)
        case view: View =>
          (List(view.from, view.to).flatMap(objects.TheoryExp.getSupport),
            List(view.path)
            )
        case d: Declaration => return select(d.parent.doc.toString)
      }
      ((theories.map(controller.getO) collect {
        case Some(th : DeclaredTheory) => th
      }).toList,(views.map(controller.getO) collect {
        case Some(v : View) => v
      }).toList)
    }
  }
}
class JThgraph extends SimpleJGraphExporter("thgraph") {
  val builder = GraphBuilder.AlignmentBuilder(true)
  val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[DeclaredTheory], List[View]) = {
      val th = Try(controller.get(Path.parse(s))) match {
        case scala.util.Success(t : DeclaredTheory) => t
        case _ => return (Nil,Nil)
      }
      var (theories,views) : (List[DeclaredTheory],List[View]) = (Nil,Nil)
      val insouts = controller.depstore.querySet(th.path,Includes | Declares | HasDomain | HasCodomain | RefersTo).toList :::
        th.getIncludes ::: th.getDeclarations.filter(_.isInstanceOf[NestedModule]).map(_.path)
      insouts.map(controller.getO).distinct.foreach{
        case Some(t : DeclaredTheory) => theories ::= t
        case Some(v : View) => views ::= v
        case _ =>
      }
      (th :: theories, views)
    }
  }
}
class JPgraph extends SimpleJGraphExporter("pgraph") {
  val builder = GraphBuilder.AlignmentBuilder(true)
  val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[DeclaredTheory], List[View]) = {
      val dpath = Try(Path.parse(s)) match {
        case scala.util.Success(d: DPath) => d
        case scala.util.Success(mp : MPath) => mp.parent
        case scala.util.Success(d : GlobalName) => d.module.parent
        case _ => return (Nil,Nil)
      }
      log("Doing " + dpath)
      (alltheories.filter(dpath <= _).map(controller.getO).collect{
        case Some(th : DeclaredTheory) => th
      },allviews.filter(dpath <= _).map(controller.getO).collect{
        case Some(v : View) => v
      })
    }
  }
  private def alltheories = {
    log("Loading theories...")
    val ret = (controller.depstore.getInds(IsTheory) collect {
      case mp: MPath => mp
    }).toList
    log("Done.")
    ret
  }
  private def allviews = {
    log("Loading views...")
    val ret = (controller.depstore.getInds(IsView) collect {
      case mp : MPath => mp
    }).toList
    log("Done.")
    ret
  }
}
class JArchiveGraph extends SimpleJGraphExporter("archivegraph") {
  val builder = GraphBuilder.AlignmentBuilder(false)
  val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[DeclaredTheory], List[View]) = {
      val a = controller.backend.getArchives.filter(_.id.startsWith(s.trim))
      var (theories,views) : (List[DeclaredTheory],List[View]) = (Nil,Nil)
      a.flatMap(_.allContent()).map(controller.getO) foreach {
        case Some(th : DeclaredTheory) => theories ::= th
        case Some(v : View) => views ::= v
        case _ =>
      }
      (theories,views)
    }
  }
}
// TODO archivegraph

abstract class JGraphNode {
  val id : String
  val style : String
  val label : Option[String]
  val uri : Option[String]
  val others : List[(String,String)] = Nil

  def toJSON = JSONObject(("id",JSONString(id)) :: ("style",JSONString(style)) ::
    List(label.map(l => ("label",JSONString(l))).toList,uri.map(i => ("url",JSONString("/?" + id))).toList,others.map(p => (p._1,JSONString(p._2)))).flatten :_*)
}

abstract class JGraphEdge {
  val id : String
  val style : String
  val from : String
  val to : String
  val label : Option[String]
  val uri : Option[String]
  val others : List[(String,String)] = Nil

  def toJSON = JSONObject(("id",JSONString(id)) :: ("style",JSONString(style)) :: ("from",JSONString(from)) :: ("to",JSONString(to)) ::
    List(label.map(l => ("label",JSONString(l))).toList,uri.map(i => ("url",JSONString("/?" + id))).toList,others.map(p => (p._1,JSONString(p._2)))).flatten :_*)
}

abstract class JGraphSelector {
  def select(s : String)(implicit controller : Controller): (List[DeclaredTheory],List[View])
}

abstract class JGraphBuilder {
  def build(theories : List[DeclaredTheory], views : List[View])(implicit controller : Controller) : JSON
}
abstract class StandardBuilder extends JGraphBuilder {
  protected def doTheory(th : DeclaredTheory)(implicit controller : Controller) : (List[JGraphNode],List[JGraphEdge])
  protected def doView(v : View)(implicit controller : Controller) : (List[JGraphNode],List[JGraphEdge])

  def build(theories : List[DeclaredTheory], views : List[View])(implicit controller : Controller) = {
    var edges : List[JGraphEdge] = Nil
    var nodes : List[JGraphNode] = Nil
    views foreach (v => doView(v) match {
      case (ns,es) =>
        nodes = nodes ::: ns
        edges = edges ::: es
    })
    theories foreach (th => doTheory(th) match {
      case (ns,es) =>
        nodes = nodes ::: ns
        edges = edges ::: es
    })

    JSONObject(("nodes",JSONArray(nodes.map(_.toJSON):_*)),("edges",JSONArray(edges.map(_.toJSON):_*)))
  }
}

object GraphBuilder {
  def standardTheory(th : DeclaredTheory,doMeta : Boolean = true) = {
    val thnode = List(new JGraphNode {
      val id = th.path.toString
      val style = "theory"
      val label = Some(th.name.toString)
      val uri = Some(th.path.toString)
    })
    val metaedge = if (doMeta && th.meta.isDefined) List(new JGraphEdge {
      val id = th.path + "?Meta"
      val style = "meta"
      val from = th.meta.get.toString
      val to = th.path.toString
      val label = None
      val uri = None
    }) else Nil
    val structedges = th.getPrimitiveDeclarations.collect {
      case s : Structure => new JGraphEdge {
        override val uri: Option[String] = Some(s.path.toString)
        override val from: String = s.from.toMPath.toString
        override val to: String = th.path.toString
        override val label: Option[String] = s.name.steps match {
          case List(ComplexStep(_)) => None
          case _ => Some(s.name.toString)
        }
        override val id: String = s.path.toString
        override val style: String = s match {
          case inc @ PlainInclude(_,_) => "include"
          case _ => "structure"
        }
      }
    }
    (thnode,metaedge ::: structedges)
  }

  def standardView(v : View) : (List[JGraphNode],List[JGraphEdge]) = {
    val fr = v.from match {
      case OMMOD(mp) => mp.toString
      case _ => return (Nil,Nil)
    }
    val t = v.to match {
      case OMMOD(mp) => mp.toString
      case _ => return (Nil,Nil)
    }
    (Nil,List(new JGraphEdge {
      val id = v.path.toString
      val style = "view"
      val from = fr
      val to = t
      val label = Some(v.name.toString)
      val uri = Some(v.path.toString)
    }))
  }

  case class PlainBuilder(doMeta : Boolean) extends StandardBuilder {
    def doView(v: View)(implicit controller : Controller) = standardView(v)
    def doTheory(th: DeclaredTheory)(implicit controller : Controller) = standardTheory(th,doMeta)
  }

  case class AlignmentBuilder(doMeta : Boolean) extends StandardBuilder {
    def doView(v: View)(implicit controller : Controller) = standardView(v)
    def doTheory(th: DeclaredTheory)(implicit controller : Controller) : (List[JGraphNode],List[JGraphEdge]) = {
      val (ths,views) = standardTheory(th,doMeta)
      val alserver = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse(return (ths,views))
      val als = th.getConstants.flatMap(c => alserver.getFormalAlignments(c.path))
      val es = als.map(al => new JGraphEdge {
        override val uri: Option[String] = None
        override val from: String = al.from.mmturi.module.toString
        override val to: String = al.to.mmturi.module.toString
        override val label: Option[String] = None
        override val id: String = "alignment_" + from + "_" + to
        override val style: String = "alignments"
      })
      (ths,views ::: es)
    }
  }
}