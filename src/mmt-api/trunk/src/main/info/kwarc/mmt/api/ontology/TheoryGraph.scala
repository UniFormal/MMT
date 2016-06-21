package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils.MyList.fromList
import scala.xml.Node
import scala.collection.mutable.{HashSet,HashMap}

/** types of edges in a theory multigraph; edges may have an id; there may be at most one edge without id between two nodes */
abstract class Edge
/** inclusion edge */
case object IncludeEdge extends Edge
/** meta-theory edge */
case object MetaEdge extends Edge
/** named view */
case class ViewEdge(id: Path) extends Edge
/** named import */
case class StructureEdge(id: Path) extends Edge

/** an edge together with its end point, optionally may be backwards */
case class EdgeTo(to: Path, edge: Edge, backwards: Boolean = false)

/** This class provides functions for rendering a theory graph in the gexf and dot graph formats.
 *  @param theories the minimal set of theories to include
 *  @param views the minimal set of views to include
 *  @param tg the theory graph from which further information is obtained
 */ 
class GraphExporter(theories: Iterable[Path], views: Iterable[Path], tg: TheoryGraph) {
   private def gexfNode(id:Path, tp: String) =
      <node id={id.toPath} label={id.last}><attvalues><attvalue for="type" value={tp}/></attvalues></node>
   private def gexfEdge(id:String, from: Path, to: Path, tp: String) =
      <edge id={id} source={from.toPath} target={to.toPath}><attvalues><attvalue for="type" value={tp}/></attvalues></edge>
   def toGEXF: Node = {
     var edgeNodes : List[Node] = Nil
     var edges : List[Node] = Nil
     theories foreach {from => tg.edgesFrom(from) foreach {case (_, etos) => etos foreach {
         case EdgeTo(_,_,true) =>
         case EdgeTo(to, ViewEdge(v), _) =>
           edgeNodes ::= gexfNode(v, "view")
           edges ::= gexfEdge("source: " + v.toPath, from, v, "source")
           edges ::= gexfEdge("target: " + v.toPath, v, to, "target")
         case EdgeTo(to, StructureEdge(s), _) =>
           edgeNodes ::= gexfNode(s, "structure")
           edges ::= gexfEdge("source: " + s.toPath, from, s, "source")
           edges ::= gexfEdge("target: " + s.toPath, s, to, "target")
         case EdgeTo(to, MetaEdge, _) =>
           edges ::= gexfEdge("meta: " + from.toPath + "--" + to.toPath, from, to, "meta")
         case EdgeTo(to, IncludeEdge, _) =>
           edges ::= gexfEdge("include: " + from.toPath + "--" + to.toPath, from, to, "include")
     }}}
      <gexf xmlns="http://www.gexf.net/1.2draft">
         <graph defaultedgetype="directed">
            <attributes>
               <attribute id="type" title="type" type="string"/>
            </attributes>
            <nodes>
              {theories map {node => gexfNode(node, "theory")}}
              {edgeNodes}
            </nodes>
            <edges>{edges}</edges>
         </graph>
      </gexf>
   }
   def exportGEXF(filename: utils.File) {
      utils.File.write(filename, toGEXF.toString)
   }
   // note graphviz does not like some characters (including -) in the tooltip attributes
   private def dotNode(id:Path, tp: String) = {
      val label = id match {
         case utils.mmt.mmtbase ? !(name)  if name.toString.startsWith("[") =>
            objects.TheoryExp.toString(objects.Obj.fromPathEncoding(name.toString))
         case _ =>
            id.last
      }
      val tooltipAtt = s"""tooltip="graph$tp""""
      val uriAtt = s"""href="${id.toPath}""""
      "\"" + id.toPath + "\" [label=\"" + label + "\"," + tooltipAtt + "," + uriAtt + "];"
   }
   private def dotEdge(id:Option[Path], from: Path, to: Path, tp: String, external: Boolean) = {
      val idAtts = id match {
         case None => s"""tooltip="graph$tp""""
         case Some(id) => s"""label="${id.last}" tooltip="graph$tp" href="${id.toPath}"""" 
      }
      val weight = if (external) 1 else 10
      "\"" + from.toPath + "\" -> \"" + to.toPath + "\" " + s"[$idAtts,weight=$weight];"
   }
   /**
    * exports in dot format
    *
    * the graph includes:
    *   all theories that have a link into/out of a minimal theory or that are domain/codomain of a minimal view
    *   all links between these theories
    *   the minimal theories are clustered
    */
   def toDot: String = {
     var res: List[String] = Nil
     // all nodes that have been added
     var nodesDone: List[Path] = Nil
     // external nodes that have been added
     var externalNodes: List[Path] = Nil
     // add the minimal theories
     res ::= "subgraph cluster_local {"
     theories.foreach {node =>
        res ::= dotNode(node, "theory")
        nodesDone ::= node
     }
     res ::= "}"
     def addNodeIfNeeded(p: Path) : Boolean = {
        if (! nodesDone.contains(p)) {
           res ::= dotNode(p, "exttheory")
           nodesDone ::= p
           externalNodes ::= p
           true
        } else
           false
     }
     // adds an edge going out of from
     def addEdgeIfNeeded(from: Path, eto: EdgeTo, external: Boolean) = eto match {
         case EdgeTo(to, ViewEdge(v), false) if ! views.exists(_ == v) =>
            res ::= dotEdge(Some(v), from, to, "view", external)
         case EdgeTo(to, StructureEdge(s), false) =>
            res ::= dotEdge(Some(s), from, to, "structure", external)
         case EdgeTo(to, MetaEdge, false) =>
            res ::= dotEdge(None,    from, to, "meta", external)
         case EdgeTo(to, IncludeEdge, false) =>
            res ::= dotEdge(None,    from, to, "include", external)
         case _ =>
     }
     // add the minimal views
     views.foreach {view =>
        val fromO = tg.domain(view)
        val toO = tg.codomain(view)
        (fromO,toO) match {
           case (Some(from),Some(to)) =>
              addNodeIfNeeded(from)
              addNodeIfNeeded(to)
              res ::= dotEdge(Some(view), from, to, "view", false)
           case _ =>
              throw GeneralError("domain/codomain of view not known: " + view)
        } 
     }
     // add all the links from/out of the minimal theories that aren't part of the minimal views
     theories.foreach {from =>
       tg.edgesFrom(from) foreach {case (to, etos) =>
          val externalNode = addNodeIfNeeded(to) // true if the partner node is from a different document
          // incoming edges (from any node)
          etos foreach {
            case EdgeTo(_, ViewEdge(v), true) if ! views.exists(_ == v) =>
               res ::= dotEdge(Some(v), to, from, "view", externalNode)
            case EdgeTo(_, StructureEdge(s), true) =>
               res ::= dotEdge(Some(s), to, from, "structure", externalNode)
            case EdgeTo(_, MetaEdge, true) =>
               res ::= dotEdge(None,    to, from, "meta", externalNode)
            case EdgeTo(_, IncludeEdge, true) =>
               res ::= dotEdge(None,    to, from, "include", externalNode)
            case _ =>
          }
          // outgoing edges (into external nodes)
          if (externalNode) etos foreach {eto => addEdgeIfNeeded(from, eto, true)}
       }
     }
     // edges between external nodes
     externalNodes.foreach {from =>
        tg.edgesFrom(from) foreach {case (to, etos) =>
           if (externalNodes.exists(_ == to)) etos foreach {eto => addEdgeIfNeeded(from, eto, true)}
        }
     }
     res.reverse.mkString("digraph MMT {\n", "\n", "}")
   }
   /** like toDot but writes to a file */
   def exportDot(filename: utils.File) {
      utils.File.write(filename, toDot)
   }
}

/** This class adds advanced queries on top of a RelStore that expose the theory graph structure */ 
class TheoryGraph(rs: RelStore) {
   /**
    * provides the nodes of the graph
    */
   def nodes : Iterator[Path] = rs.getInds(IsTheory)
   /*def edges(from: Path, to: Path) : List[Edge] = {
      var eds : List[Edge] = Nil
      rs.query(from, - HasDomain) {
         link => rs.query(link, + HasCodomain) {
            cod => if (cod == to) {
               if (rs.hasType(link, IsView)) eds ::= ViewEdge(link)
               else if (rs.hasType(link, IsStructure)) eds ::= StructureEdge(link)
            }
         }
      }
      rs.query(to, + Includes) {
         i => if (i == from) eds ::= IncludeEdge
      }
      rs.query(to, + HasMeta) {
         i => if (i == from) eds ::= MetaEdge
      }
      eds
   }*/
   /**
    * returns all edges into or out of a theory
    * @param from the theory 
    * @return all edges, backwards is set for incoming edges
    */
   def edgesFrom(from: Path) : List[(Path,List[EdgeTo])] = {
      var eds : List[EdgeTo] = Nil
      rs.query(from, - HasDomain) {
         link => rs.query(link, + HasCodomain) {
            cod =>
               if (rs.hasType(link, IsView)) eds ::= EdgeTo(cod,ViewEdge(link))
               else if (rs.hasType(link, IsStructure)) eds ::= EdgeTo(cod, StructureEdge(link))
         }
      }
      rs.query(from, - HasCodomain) {
         link => rs.query(link, + HasDomain) {
            dom =>
               if (rs.hasType(link, IsView)) eds ::= EdgeTo(dom,ViewEdge(link), true)
               else if (rs.hasType(link, IsStructure)) eds ::= EdgeTo(dom, StructureEdge(link), true)
         }
      }
      rs.query(from, - Includes) {
         i => eds ::= EdgeTo(i, IncludeEdge)
      }
      rs.query(from, + Includes) {
         i => eds ::= EdgeTo(i, IncludeEdge, true)
      }
      rs.query(from, - HasMeta) {
         i => eds ::= EdgeTo(i, MetaEdge)
      }
      rs.query(from, + HasMeta) {
         i => eds ::= EdgeTo(i, MetaEdge, true)
      }
      val filtered = eds filterNot {
        case EdgeTo(t,_,_) => from <= t
      }
      filtered.quotient(_.to)
   }
   /** return the domain of this link, if any */
   def domain(link: Path) : Option[Path] = {
      rs.query(link, +HasDomain)(p => return Some(p))
      return None
   }
   /** return the codomain of this link, if any */
   def codomain(link: Path) : Option[Path] = {
      rs.query(link, +HasCodomain)(p => return Some(p))
      return None
   }
   
}