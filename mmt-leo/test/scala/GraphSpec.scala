package scala



import org.scalatest._

import scala.collection.mutable
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphTraversal.Predecessors
import scalax.collection.mutable.Graph

class GraphSpec extends FlatSpec with Matchers {


  class TransitiveGraph{
    type Term = Int

    // NEED TO CHANGE THESE TWO IN FURTHER INSTANTIATIONS
    protected val g:Graph[Term,UnDiEdge] = Graph()

    def add(t1:Term,t2:Term, directed: Boolean = true):Unit = {
      if (t1==t2) return
      if (directed) {
        if (g.contains(DiEdge(t2, t1)) || g.contains(UnDiEdge(t1, t2)) ) {
          add(t1,t2,directed=false)
        }else{
          g += DiEdge(t1, t2)
        }
      }else{
        if (g.contains(DiEdge(t2, t1))) {g-=DiEdge(t2,t1)}
        if (g.contains(DiEdge(t1, t2))) {g-=DiEdge(t1,t2)}
        g += UnDiEdge(t1,t2)
      }
    }

    def addAndRefactor(t1:Term,t2:Term) = {
      add(t1,t2)
      refactorAt(t1)
      refactorAt(t2)
    }

    def add(l: List[(Term,Term)]):Unit = l.foreach(e=>add(e._1,e._2))
    def add(l: (Term,Term)*):Unit = l.foreach(e=>add(e._1,e._2))

    def n(outer: Term) = g get outer

    def compare(a:Term,b:Term):Option[Boolean] = {
      if (n(a).pathTo(n(b)).isDefined) return Some(true)
      None
    }

    override def toString:String = g.toString()



    def refactorAt(t1:Term) = {
      transClosureOf(t1).foreach(add(t1,_))
      dualTransClosureOf(t1).foreach(add(_,t1))
    }

    def transClosureOf(t1:Term) = n(t1).outerNodeTraverser.toSet

    def dualTransClosureOf(t1:Term)=n(t1).outerNodeTraverser.withDirection(Predecessors).toSet

    def getGraph = g
  }


  "a transitive graph" should "add edges intelligently" in {
    val tg = new TransitiveGraph
    tg.add((1, 2), (2, 3), (2, 4), (5, 4),(6,1))
    tg.transClosureOf(1) should be(Set(1, 2, 3, 4))
    tg.dualTransClosureOf(1) should be(Set(1,6))
    tg.add(3, 2)
    tg.add(3, 2)
    tg.add(2, 3)
    tg.getGraph should be(Graph(1, 2, 3, 4, 5,6,
    DiEdge(1, 2), DiEdge(2, 4), UnDiEdge(3, 2), DiEdge(5, 4),DiEdge(6, 1)))

    tg.getGraph filter tg.getGraph.having(node = _ >= 2)        // Graph(2,3,5, 2~>3)
  }

  it should "be able to eliminate cycles" in {
    val tg1= new TransitiveGraph
    tg1.add((1, 2), (2, 3), (3, 4), (4, 5))
    tg1.addAndRefactor(5,2)
    println(tg1)
  }

  it should "preserve qeue orderings" in {
    val foo1 = new mutable.Queue[Int]()
    val foo2 = new mutable.Queue[Int]()
    val foo3 = new mutable.Queue[Int]()
    foo1.enqueue(1)
    foo1.enqueue(2)
    foo1.enqueue(3)
    foo2.enqueue(4)
    foo2.enqueue(5)
    foo2.enqueue(6)
    foo3.enqueue(7)
    foo3.enqueue(8)
    foo3.enqueue(9)

    val l = List(foo1,foo2,foo3)
    val bar = new mutable.Queue[Int]()
    l.foreach(a=> a.dequeueAll(t=>true).foreach(bar.enqueue(_)))

    bar.dequeue()
    println(bar)


  }


}

