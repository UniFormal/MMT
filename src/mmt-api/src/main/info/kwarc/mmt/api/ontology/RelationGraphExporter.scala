package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import documents._
import modules._
import archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.web._
import utils._
import presentation._

/**
 * builds a graph from relational and then calls dot to produce an svg file
 */
abstract class RelationGraphExporter extends StructurePresenter {

  override def outExt = "svg"
  /** a short descriptive name to be used in labels, menus, etc. */
  def description: String

  override def toString = super.toString + s" (path to dot: $graphviz)"

  /** path to graphviz (dot) binary */
  private var graphviz: Option[String] = None
  private def getGraphviz = graphviz orElse controller.getEnvVar("GraphViz") getOrElse("dot")

  /** expects one argument: the path to graphviz; alternatively set variable GraphViz */
  override def start(args: List[String]): Unit = {
    super.start(args)
    val gv = getFromFirstArgOrEnvvar(args, "GraphViz")
    if (gv.nonEmpty)
      graphviz = Some(gv)
  }

  /**
   * build the abstract graph to be visualized
   * @param startPoints the elements around which the graph should be built
   */
  def buildGraph(se: StructuralElement): DotGraph
  // controller.depstore.querySet(container, Transitive(+Declares) * HasType(???))

  /** contains at least all elements of the document */
  def apply(se: StructuralElement, standalone: Boolean = false)(implicit rh: RenderingHandler): Unit = {
    val dg = buildGraph(se)
    val dot = new DotToSVG(File(getGraphviz))
    val svg = try {
      dot(dg)
    } catch {
      case e: Exception => throw LocalError("error while producing graph").setCausedBy(e)
    }
    rh(svg)
  }

  @MMT_TODO("")
  def asJSON(se : StructuralElement): JSONObject = {
    val dg = buildGraph(se)
    val nodes = dg.JSONNodes.toList
    val edges = dg.JSONEdges.toList//.filter(o => nodes.exists(p => p("label") == o("from")) && nodes.exists(p => p("label") == o("to")))
    JSONObject(("nodes",JSONArray(nodes:_*)),("edges",JSONArray(edges:_*)))
  }
  @MMT_TODO("")
  def asJSON(ls : List[StructuralElement]) : JSONObject = {
    val dgs = ls.map(buildGraph)
    val nodes = dgs.flatMap(_.JSONNodes).distinct
    val edges = dgs.flatMap(_.JSONEdges).distinct
    JSONObject(("nodes",JSONArray(nodes:_*)),("edges",JSONArray(edges:_*)))
  }
}

/** builds a graph containing all nodes and edges of the types */
class SimpleRelationGraphExporter(val key: String, val description: String, nodeSet: RelationExp, edgeTypes: List[Binary]) extends RelationGraphExporter {
   def buildGraph(se: StructuralElement) = new DotGraph {
     val title = "\"" + key + " for " + se.path.toString + "\""
     val nodes = {
        controller.depstore.querySet(se.path, (Declares^*) * nodeSet).map {p =>
           new DotNode {
             val id = p
             val label = p.last
             val cls = "graph"+controller.depstore.getType(p).map(_.toString).getOrElse("other")
           }
        }
     }
     val edges = {
        var res : List[DotEdge] = Nil
        nodes.foreach {f => nodes.foreach {t =>
          edgeTypes.foreach {et =>
            if (controller.depstore.hasDep(f.id, t.id, et))
              res ::= new DotEdge {
                 val from = f
                 val to = t
                 val id = None
                 val label = None
                 val cls = "graph"+et.toString
             }
          }
        }}
        res
     }
     val externalNodes = None
   }
}

class DeclarationTreeExporter extends SimpleRelationGraphExporter("decltree", "declaration tree", ((Includes | Declares)^*) * HasType(IsConstant,IsTheory), List(Includes,Declares))

class DependencyGraphExporter extends SimpleRelationGraphExporter("depgraph", "dependency graph", ((Includes | Declares)^*) * HasType(IsConstant), List(DependsOn))


/**
 * uses [[ontology.TheoryGraphFragment]] to produce a dot file and then calls dot to produce an svg file
 */
class TheoryGraphExporter extends RelationGraphExporter {
  val key = "thygraph"
  val description = "theory graph"

  private lazy val tg: ontology.TheoryGraph = new ontology.TheoryGraph(controller.depstore)

  def buildGraph(se: StructuralElement) : DotGraph = {
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
      case nm: NestedModule => return buildGraph(nm.module)
      case d: Declaration => return buildGraph(controller.get(d.parent))
    }
    val tgf = new ontology.TheoryGraphFragment(theories, views, tg, report, includeMeta = false)
    tgf.toDot
  }
}

class PathGraphExporter extends RelationGraphExporter {
  val key = "pathgraph"
  val description = "path graph"
  override val logPrefix = "pathgraph"

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
  private lazy val tg: ontology.TheoryGraph = new ontology.TheoryGraph(controller.depstore)

  def buildGraph(se: StructuralElement) : DotGraph = {
    val dpath = se match {
      case d: Document => d.path
      case mp : Module => mp.parent
      case d : Declaration => d.parent.parent
      case _ => ???
    }
    log("Doing " + dpath)
    val (theories,views) = (alltheories.filter(dpath <= _),allviews.filter(dpath <= _))

    val tgf = new ontology.TheoryGraphFragment(theories, views, tg, report)
    log("Done.")
    tgf.toDot
  }
}
