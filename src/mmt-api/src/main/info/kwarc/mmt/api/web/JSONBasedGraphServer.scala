package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension, FormatBasedExtension}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams.InstallDiagram
import info.kwarc.mmt.api.objects.{OMID, OMMOD, OMS}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation.MMTDocExporter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.util.Try

/**
  * Provides JSON representations of theory graphs (as known to MMT) at [[http://<mmt server>/:jgraph/json?key=<key>&uri=<datum>]].
  * Most prominently, this JSON endpoint is used by TGView2D at [[http://<mmt server>/graphs/tgview.html?type=<key>&graphdata=<datum>]].
  *
  * The [[JSONBasedGraphServer]] server extension hooks up with MMT's web server and replies at URIs like
  * [[http://<mmt server>/:jgraph/json?key=<key>&uri=<datum>]].
  *
  * Here, ''<key>'' selects one of the theory graphs providers declared in this file. And ''<datum>'' is some input
  * to that provider.
  * For example, the [[JArchiveGraph]] provider listens at ''<key> = archivegraph'' and expects ''<datum>'' to be an
  * archive ID.
  *
  * @example Say you wanted to query and display the whole theory graph of MMT/LATIN2.
  *          MMT/LATIN2 is a whole archive, hence you want to use the [[JArchiveGraph]] provider with datum ''MMT/LATIN2''.
  *          
  *          You can download the theory graph JSON at [[http://localhost:8080/:jgraph/json?key=archivegraph&uri=MMT/LATIN2]].
  *          You can access the theory graph visually with TGView2D by browsing at [[http://localhost:8080/graphs/tgview.html?type=archivegraph&graphdata=MMT/LATIN2]].
  *
  * @author Jazzpirate (+ docs by ComFreek)
  */

class DirectGraphBuilder extends FormatBasedExtension {
  final override def isApplicable(cont: String): Boolean = cont == "jgraph"
  override val logPrefix = "jgraph"

  private case class CatchError(s: String) extends Throwable

  final override def start(args: List[String]) {
    List(
      new JDocgraph,
      new JThgraph,
      new JPgraph,
      new JArchiveGraph,
      new JMPDGraph,
      new JDiagramGraph
    ).foreach(controller.extman.addExtension(_))
    super.start(args)
  }

  private[web] def apply(uri: String, key: String, sem: String = "none", comp: String = "default"): JSON = {
    val exp = controller.extman.getOrAddExtension(classOf[JGraphExporter], key).getOrElse {
      throw CatchError(s"exporter $key not available")
    }
    if (sem == "none") {
      log("Computing " + key + " for " + uri + "... ")
      val ret = exp.buildGraph(uri)
      log("Done")
      ret
    } else {
      log("With " + sem + " semantic " + "using " + comp + " solver " + "computing " + key + " for " + uri + "...")
      val ret = exp.computeSem(exp.buildGraph(uri), sem, comp)
      log("Done")
      ret
    }
  }
}


class JSONBasedGraphServer extends ServerExtension("jgraph") {
  final override val logPrefix = "jgraph"

  private case class CatchError(s: String) extends Throwable

  final override def start(args: List[String]) {
    controller.extman.addExtension(new JGraphSideBar)
    controller.extman.addExtension(new DirectGraphBuilder)
  }

  lazy private val sidebar = controller.extman.get(classOf[JGraphSideBar]).head
  lazy private val buil = controller.extman.get(classOf[DirectGraphBuilder]).head

  final override def apply(request: ServerRequest): ServerResponse = {
    log("Paths: " + request.pathForExtension)
    log("Query: " + request.query)
    log("Path: " + request.parsedQuery("uri"))
    log("Semantic: " + request.parsedQuery("semantic"))
    if (request.pathForExtension.headOption.contains("menu")) {
      val id = request.parsedQuery("id").getOrElse("top")
      log("Returning menu for " + id)
      if (id == "full") ServerResponse.fromJSON(sidebar.getJSON("top", full = true))
      else ServerResponse.fromJSON(sidebar.getJSON(id))
    } else if (request.pathForExtension.headOption.contains("json")) {
      val uri = request.parsedQuery("uri").getOrElse(return ServerResponse.errorResponse(LocalError("Not a URI"), "json"))
      val key = request.parsedQuery("key").getOrElse("pgraph")
      val sem = request.parsedQuery("semantic").getOrElse("none")
      val comp = request.parsedQuery("comp").getOrElse("default")
      val graph = buil(uri, key, sem, comp)
      val ret = ServerResponse.fromJSON(graph)
      ret
    } else ServerResponse.errorResponse("Invalid path", "json")
  }
}

private class JGraphSideBar extends Extension {
  private class Tree(val id: String, val str: String, val uri: String, val tp: String) {
    var children: List[Tree] = Nil

    def add(ch: Tree): Unit = if (!children.contains(ch)) {
      children ::= ch
    }

    def toJSON: JSON = JSONObject(
      ("menuText", JSONString(str)),
      ("id", JSONString(id)),
      ("uri", JSONString(uri)),
      ("type", JSONString(tp)),
      ("hasChildren", JSONBoolean(children.nonEmpty)))

    def fullJSON: JSON = JSONObject(
      ("menuText", JSONString(str)),
      ("id", JSONString(id)),
      ("uri", JSONString(uri)),
      ("type", JSONString(tp)),
      ("children", JSONArray(children.sortBy(_.str).map(_.fullJSON).distinct: _*)))
  }

  private object Tree {
    private val hm = scala.collection.mutable.HashMap.empty[String, Tree]

    def apply(id: String, str: String = "", uri: String = "", tp: String = "pathgraph"): Tree = {
      hm.getOrElse(id, {
        val ret = new Tree(id, if (str == "") id else str, uri, tp)
        hm(id) = ret
        ret
      })
    }
  }

  def getJSON(id: String, full: Boolean = false): JSON = if (id == "top") JSONArray(archs.sortBy(_.str).distinct.map(t =>
    if (full) t.fullJSON else t.toJSON): _*)
  else if (full) JSONArray(Tree(id).children.map(_.fullJSON): _*) else JSONArray(Tree(id).children.map(_.toJSON): _*)

  private def trimpath(p: Path) = if (p.toString.trim.last == '/') p.toString.trim.init else p.toString.trim

  private def doArchive(a: Archive): Tree = {
    val ret = if (a.id contains "/") {
      val List(pre, post) = utils.stringToList(a.id, "/")
      Tree(pre, pre, pre, "archivegraph").add(Tree(a.id, post, a.id, "archivegraph"))
      Tree(pre)
    } else Tree(a.id, a.id, a.id, "archivegraph")
    Tree(a.id).add(Tree(a.id + "-narr", "Narration", a.narrationBase.toString, "thgraph"))
    Tree(a.id).add(Tree(a.id + "-cont", "Content", a.id, "archivegraph"))
    doNarr(DPath(a.narrationBase)).children.foreach(Tree(a.id + "-narr").add)
    val mods = a.allContent
    mods.map(p => doCont(p, a.id)).distinct.foreach(Tree(a.id + "-cont").add)
    ret
  }

  private def doCont(p: Path, suffix: String = ""): Tree = p match {
    case mp: MPath =>
      val ret = doCont(mp.parent, suffix)
      Tree(trimpath(mp.parent) + "-cont-" + suffix).add(Tree(trimpath(mp) + "-cont-" + suffix, "?" + mp.name.toString, trimpath(mp), "thgraph"))
      ret
    case dp: DPath =>
      if (dp.ancestors.length == 1) Tree(trimpath(dp) + "-cont-" + suffix, trimpath(dp), trimpath(dp), "pgraph")
      else {
        val ret = doCont(dp.^^, suffix)
        ret.add(Tree(trimpath(dp) + "-cont-" + suffix, dp.last, trimpath(dp), "pgraph"))
        ret
      }
    case _ => throw ImplementationError("impossible case")
  }

  private def doNarr(p: Path): Tree = p match {
    case dp: DPath =>
      val doc = Try(controller.getDocument(dp)).toOption
      val s2 = doc match {
        case Some(d) if !d.root => d.path.last
        case _ => trimpath(dp)
      }
      doc.foreach(_.getDeclarations.foreach {
        case r: NRef => Tree(trimpath(dp) + "-narr", s2, trimpath(dp), "docgraph").add(doNarr(r.target))
        case doc: Document => Tree(trimpath(dp) + "-narr", s2, trimpath(dp), "docgraph").add(doNarr(doc.path))
        case _ =>
      })
      Tree(trimpath(dp) + "-narr", s2, trimpath(dp), "docgraph")
    case mp: MPath =>
      Tree(trimpath(mp) + "-narr", "?" + mp.name.toString, trimpath(mp), "thgraph")
    case _ => Tree("")
  }

  private def archs = controller.backend.getArchives.sortBy(_.id).map(doArchive)

  final override def start(args: List[String]) {
    super.start(args)
    // lazy private val top = archs.map(doArchive).sortBy(_._1).distinct.map(p => (p._1,p._2.toJSON))
  }
}

abstract class JGraphExporter(val key: String) extends FormatBasedExtension {
  def isApplicable(format: String): Boolean = format == key

  def buildGraph(s: String): JSON

  def computeSem(f: JSON, sem: String, comp: String): JSON
}

abstract class SimpleJGraphExporter(key: String) extends JGraphExporter(key) {
  override def logPrefix: String = key

  protected val builder: JGraphBuilder
  protected val selector: JGraphSelector

  def buildGraph(s: String): JSON = {
    val (ths, vs, docs) = selector.select(s)(controller)
    log("building...")
    val res = builder.build(ths, vs, docs)(controller)
    log("Done.")
    res
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


private class JDocgraph extends SimpleJGraphExporter("docgraph") {
  final override protected val builder = GraphBuilder.PlainBuilder
  final override protected val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[Theory], List[View], List[Document]) = {
      val se = Try(controller.get(Path.parse(s))) match {
        case scala.util.Success(e: StructuralElement) => e
        case _ => return (Nil, Nil,Nil)
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
        case Some(th: Theory) => th
      }).toList, (views.map(controller.getO) collect {
        case Some(v: View) => v
      }).toList,Nil)
    }
  }
}

private class JThgraph extends SimpleJGraphExporter("thgraph") {
  final override protected val builder = GraphBuilder.AlignmentBuilder(log(_, None))
  final override protected val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[Theory], List[View],List[Document]) = {
      val th = Try(controller.get(Path.parse(s))) match {
        case scala.util.Success(t: Theory) => t
        case _ => return (Nil, Nil, Nil)
      }
      var (theories, views): (List[Theory], List[View]) = (Nil, Nil)
      val insouts = controller.depstore.querySet(th.path, Includes | Declares | HasDomain | HasCodomain | RefersTo).toList :::
        th.getIncludes ::: th.getDeclarations.filter(_.isInstanceOf[NestedModule]).map(_.path)
      insouts.map(controller.getO).distinct.foreach {
        case Some(t: Theory) => theories ::= t
        case Some(v: View) => views ::= v
        case _ =>
      }
      (th :: theories, views,Nil)
    }
  }
}

private class JPgraph extends SimpleJGraphExporter("pgraph") {
  final override protected val builder = GraphBuilder.AlignmentBuilder(log(_, None))
  final override protected val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[Theory], List[View],List[Document]) = {
      val dpath = Try(Path.parse(s)) match {
        case scala.util.Success(d: DPath) => d
        case scala.util.Success(mp: MPath) => mp.parent
        case scala.util.Success(d: GlobalName) => d.module.parent
        case _ => return (Nil, Nil,Nil)
      }
      log("Doing " + dpath)
      (alltheories.filter(dpath <= _).map(controller.getO).collect {
        case Some(th: Theory) => th
      }, allviews.filter(dpath <= _).map(controller.getO).collect {
        case Some(v: View) => v
      }, Nil)
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
      case mp: MPath => mp
    }).toList
    log("Done.")
    ret
  }
}

/**
  * Accepts as graph data an MPath to a [[InstallDiagram diagram derived module]] as string and reads out
  * the generated theories/views from its dfC.normalized.
  *
  * @example Request with TGView 2D: [[http://localhost:8080/graphs/tgview.html?type=diaggraph&graphdata=latin:/algebraic/diagop-test?PlayTest]],
  *          where ''latin:/algebraic/diagop-test?PlayTest'' references the diagram derived module:
  * {{{
  *   namespace latin:/algebraic/diagop-test ❚
  *   diagram PlayTest : ?AlgebraicDiagOps := ... ❚
  * }}}
  */
private class JDiagramGraph extends SimpleJGraphExporter("diaggraph") {
  final override protected val builder: JGraphBuilder = GraphBuilder.PlainBuilder
  final override protected val selector: JGraphSelector = new JGraphSelector {
    override def select(s: String)(implicit controller: Controller): (List[Theory], List[View],List[Document]) = {
      try {
        val paths = InstallDiagram.parseOutput(Path.parseM(s))(controller.globalLookup).modules

        val (theories, views) = {
          val modules = paths.map(controller.get)
          (modules.collect { case x: Theory => x }, modules.collect { case x: View => x })
        }
        (theories, views, Nil)
      } catch {
        case err@(_: ParseError | _: GetError) =>
          err.printStackTrace()
          (Nil, Nil, Nil)
      }
    }
  }
}

private class JArchiveGraph extends SimpleJGraphExporter("archivegraph") {
  final override protected val builder = GraphBuilder.AlignmentBuilder(log(_, None))
  final override protected val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[Theory], List[View], List[Document]) = {
      val as = s.toString.split(""" """).map(_.trim).filter(_ != "")
      val a = controller.backend.getArchives.filter(a => as.exists(a.id.startsWith))
      log("Archives: " + a.map(_.id).mkString(", "))
      var (theories, views): (List[Theory], List[View]) = (Nil, Nil)
      a.flatMap(_.allContent).map(c => Try(controller.get(c)).toOption) foreach {
        case Some(th: Theory) => theories ::= th
        case Some(v: View) => views ::= v
        case _ =>
      }
      log(theories.length + " selected")
      (theories, views, Nil)
    }
  }
}

private class JMPDGraph extends SimpleJGraphExporter("mpd") {
  final override val logPrefix = "mpd"
  private lazy val mathpres = controller.extman.get(classOf[MMTDocExporter]).headOption.getOrElse {
    val pr = new MMTDocExporter
    controller.extman.addExtension(pr)
    pr
  }
  final override protected val builder = new StandardBuilder {
    def doView(v: View)(implicit controller: Controller) = (Nil, Nil)
    def doDocument(d: Document)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge]) = (Nil, Nil)

    def doTheory(th: Theory)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge]) = {
      log("Doing theory " + th.path)
      val consts = th.getPrimitiveDeclarations.collect { case c: Constant => c }
      val sb = new info.kwarc.mmt.api.presentation.StringBuilder
      val const = consts.find(_.rl contains "Law") match {
        case Some(c) => Some(c)
        case _ => consts.find(_.rl contains "BoundaryCondition") match {
          case Some(c) => Some(c)
          case _ => consts.find(_.rl contains "Quantity")
        }
      }
      if (const.isEmpty) return (Nil, Nil)

      val ostyle: String = const.map(c => {
        c.rl.get match {
          case "Law" => "model"
          case "BoundaryCondition" => "boundarycondition"
          case "Accepted" => "acceptedtheory"
          case "Rejected" => "rejectedtheory"
          case "Undecided" => "undecidedtheory"
          case _ => "theory"
        }
      }).get

      ostyle match {
        case "model" => const.foreach(c => mathpres(c.tp.get, Some(c.path $ TypeComponent))(sb))
        case "boundarycondition" => const.foreach(c => mathpres(c.tp.get, Some(c.path $ TypeComponent))(sb))
        case _ => const.foreach(c => mathpres(OMS(c.path), None)(sb))
      }
      log("Const: " + const)
      log("Style: " + ostyle)
      val thnode = List(new JGraphNode {
        val id = th.path.toString
        val style = ostyle
        val label = Some(th.name.toString)
        val mathml = sb.get
        val uri = Some(th.path.toString)
        override val others = List(("mathml", mathml))
      })
      val structedges = th.getPrimitiveDeclarations.collect {
        case s: Structure => new JGraphEdge {
          override val uri: Option[String] = Some(s.path.toString)
          override val from: String = s.from.toMPath.toString
          override val to: String = th.path.toString
          override val label: Option[String] = s.name.steps match {
            case List(ComplexStep(_)) => None
            case _ => Some(s.name.toString)
          }
          override val id: String = s.path.toString
          override val style: String = "modelinclude"
        }
      }
      (thnode, structedges)
    }
  }

  final override protected val selector = new JGraphSelector {
    def select(s: String)(implicit controller: Controller): (List[Theory], List[View],List[Document]) = {
      val th = Try(controller.get(Path.parse(s))) match {
        case scala.util.Success(t: Theory) => List(t)
        case _ =>
          val as = s.toString.split(""" """).map(_.trim).filter(_ != "")
          val a = controller.backend.getArchives.filter(a => as.exists(a.id.startsWith))
          log("Archives: " + a.map(_.id).mkString(", "))
          var theories: List[Theory] = Nil
          a.flatMap(_.allContent).map(c => Try(controller.get(c)).toOption) foreach {
            case Some(th: Theory) => theories ::= th
            case _ =>
          }
          log(theories.length + " selected")
          theories
      }
      var (theories, views): (List[Theory], List[View]) = (Nil, Nil)

      def recurse(ith: Theory) {
        log("Select: " + s)
        theories ::= ith
        val ins = ith.getIncludesWithoutMeta ::: ith.getNamedStructures.collect {
          case st if st.from.isInstanceOf[OMID] => st.from.toMPath
        }
        ins.map(controller.getO).distinct.foreach {
          case Some(t: Theory) if !theories.contains(t) =>
            recurse(t)
          case Some(v: View) => views ::= v
          case _ =>
        }
      }

      th foreach recurse
      //log("Selecting " + (th.path :: theories.map(_.path)).mkString(", "))
      (theories.distinct, views, Nil)
    }
  }
}

abstract class JGraphNode {
  val id: String
  val style: String
  val label: Option[String]
  val uri: Option[String]
  val others: List[(String, String)] = Nil

  def toJSON: JSON = JSONObject(("id", JSONString(id)) :: ("style", JSONString(style)) ::
    List(label.map(l => ("label", JSONString(l))).toList, uri.map(i => ("url", JSONString("/?" + id))).toList, others.map(p => (p._1, JSONString(p._2)))).flatten: _*)
}

abstract class JGraphEdge {
  val id: String
  val style: String
  val from: String
  val to: String
  val label: Option[String]
  val uri: Option[String]
  val others: List[(String, String)] = Nil

  def toJSON: JSON = JSONObject(("id", JSONString(id)) :: ("style", JSONString(style)) :: ("from", JSONString(from)) :: ("to", JSONString(to)) ::
    List(label.map(l => ("label", JSONString(l))).toList, uri.map(i => ("url", JSONString("/?" + id))).toList, others.map(p => (p._1, JSONString(p._2)))).flatten: _*)
}

abstract class JGraphSelector {
  def select(s: String)(implicit controller: Controller): (List[Theory], List[View], List[Document])
}

abstract class JGraphBuilder {
  def log(s: String) {}

  def build(theories: List[Theory], views: List[View], documents: List[Document])(implicit controller: Controller): JSON
}

abstract class StandardBuilder extends JGraphBuilder {
  protected def doTheory(th: Theory)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge])

  protected def doView(v: View)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge])

  protected def doDocument(d : Document)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge])

  def build(theories: List[Theory], views: List[View], documents:List[Document])(implicit controller: Controller): JSON = {
    var edges: List[JGraphEdge] = Nil
    var nodes: List[JGraphNode] = Nil
    views foreach (v => doView(v) match {
      case (ns, es) =>
        nodes = nodes ::: ns
        edges = edges ::: es
    })
    theories.indices foreach (th => {
      log({
        th + 1
      } + "/" + theories.length)
      doTheory(theories(th))
    } match {
      case (ns, es) =>
        nodes = nodes ::: ns
        edges = edges ::: es
    })
    documents.indices foreach (doc => {
      log({
        doc + 1
      } + "/" + documents.length)
      doDocument(documents(doc))
    } match {
      case (ns, es) =>
        nodes = nodes ::: ns
        edges = edges ::: es
    })

    JSONObject(("nodes", JSONArray(nodes.map(_.toJSON): _*)), ("edges", JSONArray(edges.map(_.toJSON): _*)))
  }
}

object GraphBuilder {
  def standardDocument(doc : Document): (List[JGraphNode], List[JGraphEdge]) = {(Nil,Nil)}

  def standardTheory(th: Theory) = {
    val thnode = List(new JGraphNode {
      val id = th.path.toString
      val style = "theory"
      val label = Some(th.name.toString)
      val uri = Some(th.path.toString)
    })
    val metaedge = if (th.meta.isDefined) List(new JGraphEdge {
      val id = th.path + "?Meta"
      val style = "meta"
      val from = th.meta.get.toString
      val to = th.path.toString
      val label = None
      val uri = None
    }) else Nil
    val structedges = th.getPrimitiveDeclarations.collect {
      case s: Structure => new JGraphEdge {
        override val uri: Option[String] = Some(s.path.toString)
        override val from: String = s.from.toMPath.toString
        override val to: String = th.path.toString
        override val label: Option[String] = s.name.steps match {
          case List(ComplexStep(_)) => None
          case _ => Some(s.name.toString)
        }
        override val id: String = s.path.toString
        override val style: String = s match {
          case inc@PlainInclude(_, _) => "include"
          case _ => "structure"
        }
      }
    }
    (thnode, metaedge ::: structedges)
  }

  def standardView(v: View): (List[JGraphNode], List[JGraphEdge]) = {
    val fr = v.from match {
      case OMMOD(mp) => mp.toString
      case _ => return (Nil, Nil)
    }
    val t = v.to match {
      case OMMOD(mp) => mp.toString
      case _ => return (Nil, Nil)
    }
    (Nil, List(new JGraphEdge {
      val id = v.path.toString
      val style = "view"
      val from = fr
      val to = t
      val label = Some(v.name.toString)
      val uri = Some(v.path.toString)
    }))
  }

  case object PlainBuilder extends StandardBuilder {
    def doView(v: View)(implicit controller: Controller) = standardView(v)

    def doTheory(th: Theory)(implicit controller: Controller) = standardTheory(th)

    def doDocument(d: Document)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge]) = standardDocument(d)
  }

  case class AlignmentBuilder(ilog: String => Unit) extends StandardBuilder {
    def doView(v: View)(implicit controller: Controller) = standardView(v)

    def doDocument(d: Document)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge]) = standardDocument(d)

    override def log(s: String): Unit = ilog(s)

    def doTheory(th: Theory)(implicit controller: Controller): (List[JGraphNode], List[JGraphEdge]) = {
      val (ths, views) = standardTheory(th)
      val alserver = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse(return (ths, views))
      val als = th.getConstants.flatMap(c => alserver.getFormalAlignments(c.path)).view
      val als2 = als.map(a => (a.from.mmturi.module.toString, a.to.mmturi.module.toString, a))
      val algroups = als2.map(t => (t._1, t._2)).distinct.map(p => (p._1, p._2, als2.filter(t => t._1 == p._1 && t._2 == p._2).map(_._3)))
      val es = algroups.map(al => new JGraphEdge {
        override val uri: Option[String] = None
        override val from: String = al._1
        override val to: String = al._2
        override val label: Option[String] = None
        override val id: String = "alignment_" + from + "_" + to
        override val style: String = "alignment"
        val clickText = al._3.map(_.toString).mkString("""<br>""")
        override val others = List(("clickText", clickText))
      }).toList
      (ths, views ::: es)
    }
  }

}

abstract class GraphSolverExtension extends Extension {
  /** An abstract class for graph solver extensions.
    * key: identifier for the extension handler.
    * graph: a theory graph json.
    * semantics: an argumentation semantic.
    * comp: the solver to be used.
    *
    */
  val key: String

  def apply(graph: JSON, semantics: String, comp: String): JSON
}
