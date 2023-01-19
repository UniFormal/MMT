package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import scala.xml.Node
import scala.collection.mutable.{HashSet,HashMap}

/** types of edges in a theory multigraph; edges may have an id; there may be at most one edge without id between two nodes */
sealed abstract class Edge
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

/** This class provides functions for rendering a theory graph fragment in the gexf and dot graph formats.
 *  @param theories the minimal set of theories to include
 *  @param views the minimal set of views to include
 *  @param tg the theory graph from which further information is obtained
 */
class TheoryGraphFragment(theories: Iterable[Path], views: Iterable[Path], tg: TheoryGraph) {
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
   def exportGEXF(filename: utils.File): Unit = {
      utils.File.write(filename, toGEXF.toString)
   }
   // note graphviz does not like some characters (including -) in the tooltip attributes
   /**
    * exports in dot format
    *
    * the graph includes:
    *   all theories that have a link into/out of a minimal theory or that are domain/codomain of a minimal view
    *   all links between these theories
    *   the minimal theories are clustered
    */
   def toDot: DotGraph = {
     // variables holding the graph data
     var nds: List[DotNode] = Nil
     var extnds: List[DotNode] = Nil
     var edgs: List[DotEdge] = Nil

     // methods for adding elements to the graph
     def addNode(p:Path, tp: String, external: Boolean) = {
        val l = p match {
           case utils.mmt.mmtbase ? !(name)  if name.toString.startsWith("[") =>
              objects.TheoryExp.toString(objects.Obj.fromPathEncoding(name.toString))
           case _ =>
              p.last
        }
        /*val tooltipAtt = s"""tooltip="graph$tp""""
        val uriAtt = s"""href="${p.toPath}""""
        "\"" + p.toPath + "\" [label=\"" + label + "\"," + tooltipAtt + "," + uriAtt + "];"*/
        val n = new DotNode {
          val id = p
          val label = l
          val cls = "graph"+tp
        }
        if (external)
          extnds ::= n
        else
          nds ::= n
     }
     def addNodeIfNeeded(p: Path) : Boolean = {
        if (! nds.exists(_.id == p) && ! extnds.exists(_.id == p)) {
           addNode(p, "exttheory", true)
           true
        } else
           false
     }
     def getNode(p: Path) = (nds.find(_.id == p) orElse extnds.find(_.id == p)).get
     def addEdge(p:Option[Path], f: Path, t: Path, tp: String, external: Boolean) = {
       val e = new DotEdge {
         val id = p
         val from = getNode(f)
         val to = getNode(t)
         val label = p.map(_.last)
         val cls = "graph"+tp
         override val weight = if (external) 1 else 10
       }
       edgs ::= e
     }
     // adds an edge going out of from
     def addEdgeIfNeeded(from: Path, eto: EdgeTo, external: Boolean) = eto match {
         case EdgeTo(to, ViewEdge(v), false) if ! views.exists(_ == v) =>
            addEdge(Some(v), from, to, "view", external)
         case EdgeTo(to, StructureEdge(s), false) =>
            addEdge(Some(s), from, to, "structure", external)
         case EdgeTo(to, MetaEdge, false) =>
            addEdge(None,    from, to, "meta", external)
         case EdgeTo(to, IncludeEdge, false) =>
            addEdge(None,    from, to, "include", external)
         case _ =>
     }

     // building the graph

     // internal theories
     theories.foreach {node =>
        addNode(node, "theory", false)
     }

     // minimal views
     views.foreach {view =>
        val fromO = tg.domain(view)
        val toO = tg.codomain(view)
        (fromO,toO) match {
           case (Some(from),Some(to)) =>
              addNodeIfNeeded(from)
              addNodeIfNeeded(to)
              addEdge(Some(view), from, to, "view", false)
           case _ =>
              throw GeneralError("domain/codomain of view not loaded (did you load the relational data?): " + view)
        }
     }

     // all the links from/out of the minimal theories that aren't part of the minimal views
     // TODO make more preactical choices for enriching the graph
     theories.foreach {from =>
       tg.edgesFrom(from) foreach {case (to, etos) =>
          val externalNode = addNodeIfNeeded(to) // true if the partner node is from a different document
          // incoming edges (from any node)
          etos foreach {
            case EdgeTo(_, ViewEdge(v), true) if ! views.exists(_ == v) =>
               addEdge(Some(v), to, from, "view", externalNode)
            case EdgeTo(_, StructureEdge(s), true) =>
               addEdge(Some(s), to, from, "structure", externalNode)
            case EdgeTo(_, MetaEdge, true) =>
               addEdge(None,    to, from, "meta", externalNode)
            case EdgeTo(_, IncludeEdge, true) =>
               addEdge(None,    to, from, "include", externalNode)
            case _ =>
          }
          // outgoing edges (into external nodes)
          if (externalNode) etos foreach {eto => addEdgeIfNeeded(from, eto, true)}
       }
     }

     // edges between external nodes
     extnds.foreach {from =>
        tg.edgesFrom(from.id) foreach {case (to, etos) =>
           if (extnds.exists(_.id == to)) etos foreach {eto => addEdgeIfNeeded(from.id, eto, true)}
        }
     }

     // build the final graph
     new DotGraph {
       val title = "MMT"
       val nodes = nds
       val edges = edgs
       val externalNodes = Some(extnds)
     }
   }
}
