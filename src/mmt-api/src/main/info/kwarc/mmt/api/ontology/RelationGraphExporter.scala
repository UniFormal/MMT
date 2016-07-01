package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import documents._
import modules._
import archives._
import utils._

/**
 * builds a graph from relational and then calls dot to produce an svg file
 */
abstract class RelationGraphExporter extends Exporter {
  
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
  def exportDocument(doc: Document, bt: BuildTask) {
    val dg = buildGraph(doc)
    exportGraph(dg, bt)
  }

  /** contains at least the theory */
  def exportTheory(thy: DeclaredTheory, bt: BuildTask) {
    val dg = buildGraph(thy)
    exportGraph(dg, bt)
  }

  /** contains at least domain, codomain, and view */
  def exportView(view: DeclaredView, bt: BuildTask) {
    //val theories = List(view.from, view.to).flatMap(objects.TheoryExp.getSupport)
    val dg = buildGraph(view)
    exportGraph(dg, bt)
  }

  /** nothing for now */
  def exportNamespace(dpath: DPath, bt: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}

  private def exportGraph(dg: DotGraph, bt: BuildTask) {
    val dot = new DotToSVG(File(graphviz))
    val svg = try {
      dot(dg)
    } catch {
      case e: Exception => throw LocalError("error while producing graph").setCausedBy(e)
    }
    File.write(bt.outFile, svg)
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

class DeclarationTreeGraphExporter extends SimpleRelationGraphExporter("decltree", ((Includes | Declares)^*) * HasType(IsConstant,IsTheory), List(Includes,Declares))

class DependencyGraphExporter extends SimpleRelationGraphExporter("depgraph", ((Includes | Declares)^*) * HasType(IsConstant), List(DependsOn))


/**
 * uses [[ontology.TheoryGraphFragment]] to produce a dot file and then calls dot to produce an svg file
 */
class TheoryGraphExporter extends RelationGraphExporter {
  val key = "svg"

  private lazy val tg: ontology.TheoryGraph = new ontology.TheoryGraph(controller.depstore)

  def buildGraph(se: StructuralElement) = {
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
    }
    val tgf = new ontology.TheoryGraphFragment(theories, views, tg)
    tgf.toDot
  }
}
