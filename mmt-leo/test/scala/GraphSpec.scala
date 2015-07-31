package scala

import org.scalatest._

import scalax.collection.Graph
// or scalax.collection.mutable.Graph
import scalax.collection.edge.Implicits._

/**
 * Created by mark on 6/25/15.
 *
 * this class represents the series of unit tests the prover
 * architecture and tree architecture must pass
 *
 */

class GraphSpec extends FlatSpec with Matchers {

  "this" should "work" in {

    val g = Graph((1~+>2)("This"), (2~+>3)("This"),(2~+>4)("That"))
    def n(outer: Int): g.NodeT = g get outer  // look up a node known to be contained

    println(n(1).withSubgraph(edges=_.label == "This") pathTo n(4)    )                  // Some(Path(1, 1~>3 %5, 3, 3~4 %1, 4))

  }


}



