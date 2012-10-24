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

/** This class adds advanced queries on top of a RelStore that expose the theory graph structure */ 
class TheoryGraph(rs: RelStore) {
   def nodes : Iterator[Path] = rs.getInds(IsTheory)
   def edges(from: Path, to: Path) : List[Edge] = {
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
   }
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
   
   private def gexfNode(id:Path, tp: String) =
      <node id={id.toPath} label={id.last}><attvalues><attvalue for="type" value={tp}/></attvalues></node>
   private def gexfEdge(id:String, from: Path, to: Path, tp: String) =
      <edge id={id} source={from.toPath} target={to.toPath}><attvalues><attvalue for="type" value={tp}/></attvalues></edge>
   def toGEXF: Node = {
     var edgeNodes : List[Node] = Nil
     var edges : List[Node] = Nil
     nodes foreach {from => edgesFrom(from) foreach {case (_, etos) => etos foreach {
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
              {nodes map {node => gexfNode(node, "theory")}}
              {edgeNodes}
            </nodes>
            <edges>{edges}</edges>
         </graph>
      </gexf>
   }
   def exportGEXF(filename: utils.File) {
      val file = utils.File.Writer(filename)
      file.write(toGEXF.toString)
      file.close
   }
   private def dotNode(id:Path, tp: String) = {
      val label = id match {
         case utils.mmt.mmtbase ? !(name) =>
            objects.TheoryExp.toString(objects.Obj.fromPathEncoding(name))
         case _ =>
            id.last
      }
      val tooltipAtt = "tooltip=\"" + tp + " " + id.toPath + "\""
      val uriAtt = "href=\"javascript:parent.navigate('" + id.toPath + "')\""
      "\"" + id.toPath + "\" [label=\"" + label + "\"," + tooltipAtt + "," + uriAtt + "];\n"
   }
   private def dotEdge(id:Option[Path], from: Path, to: Path, tp: String) = {
      val idAtts = id match {
         case None => "tooltip=\"" + tp + "\""
         case Some(id) => "label=\"" + id.last + "\", tooltip=\"" + tp + " " + id.toPath + "\"" + 
           "href=\"javascript:parent.navigate('" + id.toPath + "')\"" 
      }
      val styleAtts = tp match {
         case "view" => "style=dashed,color=\"blue:blue\""
         case "structure" => "style=bold,color=red"
         case "include" => "style=solid,color=black"
         case "meta" => "style=solid,color=green"
      }
      "\"" + from.toPath + "\" -> \"" + to.toPath + "\" [ " + idAtts + "," + styleAtts + "];\n"
   }
   def toDot: String = {
     var res: List[String] = Nil
     var nodesDone: List[Path] = Nil
     nodes.foreach {node =>
        res ::= dotNode(node, "theory")
        nodesDone ::= node
     }
     def addNodeIfNeeded(p: Path) {if (! nodesDone.contains(p))
        res ::= dotNode(p, "theory")
        nodesDone ::= p
     }
     nodes.foreach {from =>
       addNodeIfNeeded(from)
       edgesFrom(from) foreach {case (to, etos) =>
          addNodeIfNeeded(to)
          etos foreach {
            case EdgeTo(_,_,true) =>
            case EdgeTo(_, ViewEdge(v), _) =>      res ::= dotEdge(Some(v), from, to, "view")
            case EdgeTo(_, StructureEdge(s), _) => res ::= dotEdge(Some(s), from, to, "structure")
            case EdgeTo(_, MetaEdge, _) =>         res ::= dotEdge(None,    from, to, "meta")
            case EdgeTo(_, IncludeEdge, _) =>      res ::= dotEdge(None,    from, to, "include")
          }
       }
     }
     res.reverse.mkString("digraph MMT {\n", "", "}")
   }
   def exportDot(filename: utils.File) {
      val file = utils.File.Writer(filename)
      file.write(toDot)
      file.close
   }
}