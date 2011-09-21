package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._

import scala.collection.mutable.{HashSet,HashMap}

/** types of edges in a theory multigraph; edges may have an id; there may be at most one edge without id between two nodes */
abstract class Edge
case object IncludeEdge extends Edge
case object MetaEdge extends Edge
case class ViewEdge(id: Path) extends Edge
case class StructureEdge(id: Path) extends Edge

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
}