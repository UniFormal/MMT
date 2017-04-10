package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import documents._
import modules._
import archives._
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.web._
import utils._
import presentation._
import tiscaf.{HLet, HReqData}

/**
 * builds a graph from relational and then calls dot to produce an svg file
 */
abstract class RelationGraphExporter extends StructurePresenter {
  
  override def outExt = "svg"
  
  /** path to graphviz (dot) binary */
  private var graphviz: String = "dot"

  /** expects one argument: the path to graphviz; alternatively set variable GraphViz */
  override def start(args: List[String]) {
    super.start(args)
    graphviz = getFromFirstArgOrEnvvar(args, "GraphViz", graphviz)
  }

  /** 
   * build the abstract graph to be visualized
   * @param startPoints the elements around which the graph should be built
   */
  def buildGraph(se: StructuralElement): DotGraph
  // controller.depstore.querySet(container, Transitive(+Declares) * HasType(???))
  
  /** contains at least all elements of the document */
  def apply(se: StructuralElement, standalone: Boolean = false)(implicit rh: RenderingHandler) {
    val dg = buildGraph(se)
    val dot = new DotToSVG(File(graphviz))
    val svg = try {
      dot(dg)
    } catch {
      case e: Exception => throw LocalError("error while producing graph").setCausedBy(e)
    }
    rh(svg)
  }

  def asJSON(se : StructuralElement): JSONObject = {
    val dg = buildGraph(se)
    val nodes = dg.JSONNodes.toList
    val edges = dg.JSONEdges.toList//.filter(o => nodes.exists(p => p("label") == o("from")) && nodes.exists(p => p("label") == o("to")))
    JSONObject(("nodes",JSONArray(nodes:_*)),("edges",JSONArray(edges:_*)))
  }
  def asJSON(ls : List[StructuralElement]) : JSONObject = {
    val dgs = ls.map(buildGraph)
    val nodes = dgs.flatMap(_.JSONNodes).distinct
    val edges = dgs.flatMap(_.JSONEdges).distinct
    JSONObject(("nodes",JSONArray(nodes:_*)),("edges",JSONArray(edges:_*)))
  }
}

/** builds a graph containing all nodes and edges of the types */
class SimpleRelationGraphExporter(val key: String, nodeSet: RelationExp, edgeTypes: List[Binary]) extends RelationGraphExporter {
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

class DeclarationTreeExporter extends SimpleRelationGraphExporter("decltree", ((Includes | Declares)^*) * HasType(IsConstant,IsTheory), List(Includes,Declares))

class DependencyGraphExporter extends SimpleRelationGraphExporter("depgraph", ((Includes | Declares)^*) * HasType(IsConstant), List(DependsOn))


/**
 * uses [[ontology.TheoryGraphFragment]] to produce a dot file and then calls dot to produce an svg file
 */
class TheoryGraphExporter extends RelationGraphExporter {
  val key = "thygraph"

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
      case d: Declaration => return buildGraph(controller.get(d.parent.doc))
    }
    val tgf = new ontology.TheoryGraphFragment(theories, views, tg)
    tgf.toDot
  }
}

class JsonGraphExporter extends ServerExtension("fancygraph") {
 override val logPrefix = "fancygraph"
//  log("init")
  def doJSON(path : Path, exp : RelationGraphExporter) : HLet = path match {
      /*
    case d : DPath =>
      val allTheories = controller.depstore.getInds(IsTheory).flatMap {
          case mp : MPath if d <= mp =>
            controller.getO(mp) match {
              case Some(th : DeclaredTheory) => Some(th)
              case _ => None
            }
          case _ => None
        }
      log("Theories: " + allTheories.map(_.name).mkString(", "))
      Server.JsonResponse(exp.asJSON(allTheories.toList))
      */
    case _ =>
      controller.getO(path) match {
        case Some(s) =>
          println("Doing " + s.path)
          Server.JsonResponse(exp.asJSON(s))
        case _ => Server.plainErrorResponse(GetError(path.toString))
      }
  }
  def apply(request: Request): HLet = {
    log("Paths: " + request.path)
    log("Query: " + request.query)
    val path = Path.parse(request.query.trim, controller.getNamespaceMap)
    val (json,key) = if (request.path.headOption == Some("json")) (true,request.path.tail.headOption.getOrElse("svg"))
      else (false,request.path.headOption.getOrElse("svg"))
    lazy val exp = controller.extman.getOrAddExtension(classOf[RelationGraphExporter], key).getOrElse {
      throw LocalError(s"svg file does not exist and exporter $key not available: ${request.path}")
    }
    log("Returning " + {if (json) "json" else "fail"} + " for " + path)
    val ret = doJSON(path,exp)
    log("Output: " + ret.toString)
    if (json) doJSON(path,exp) else ???
  }
}