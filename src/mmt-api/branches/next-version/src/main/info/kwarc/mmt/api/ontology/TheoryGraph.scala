package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils.MyList.fromList
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
}