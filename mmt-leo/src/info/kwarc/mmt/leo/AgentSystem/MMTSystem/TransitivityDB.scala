package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import java.lang.IllegalArgumentException

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{Obj, Term}

import scala.collection.mutable
import scalax.collection.GraphTraversal.Predecessors
import scalax.collection.edge.{LDiEdge, LUnDiEdge}
import scalax.collection.mutable.Graph

/**
 * Created by Mark on 8/3/2015.
 *
 * this class holds the transitivity database which
 * stores a graph of MMTTerms related by a
 * transitive relation. All terms are labeled
 * with their respective goals to ensure soundness
 */

class TransitivityDB {
  
  protected var graphs: List[RelationGraph] = Nil

  def getGraph(globalName: GlobalName) = {
    val found = graphs.find {
      case TransitiveGraph(name) if name == globalName => true
      case EqualityGraph(name) if name == globalName => true
      case _ => false
    }
    if (found.isEmpty) {
      throw new IllegalArgumentException("No graph found")
    }
    found.get
  }


  def hasRelation(globalName: GlobalName) = {
    try {
      getGraph(globalName); true
    } catch {case e:IllegalArgumentException => false}
  }

  def addRelation(globalName: GlobalName,equality:Boolean=false) = {
    if (!hasRelation(globalName)) {
      if (equality)
        graphs ::= new EqualityGraph(globalName)
      else
        graphs ::= new TransitiveGraph(globalName)
    }
  }

}


case class TransitivityEntry(goal: Goal, tp: Term) {

  override def toString = tp.toString + "\n     " + tp.toString

  def present(presentObj: Obj => String) = {
    presentObj(tp) + " by " + presentObj(tp)
  }
}


abstract class RelationGraph(globalName: GlobalName) {

  protected val graph:Graph[TransitivityEntry,LUnDiEdge] = Graph().asInstanceOf[Graph[TransitivityEntry,LUnDiEdge]]

  def getGraph = graph
  val directed:Boolean


  def add(x:Term,y:Term, f:Fact): Unit ={
    val tx = TransitivityEntry(f.goal,x)
    val ty = TransitivityEntry(f.goal,y)
    add(tx,ty,f)
  }
  
  def add(t1:TransitivityEntry,t2:TransitivityEntry,f:Fact):Unit = {
    if (t1==t2) return
    
    if (directed) {
      graph += LDiEdge(t1, t2)(f)
    }else{
      graph += LUnDiEdge(t1,t2)(f)
    }
  }

  def add(l: List[(TransitivityEntry,TransitivityEntry,Fact)]):Unit = l.foreach(e=>add(e._1,e._2,e._3))
  def add(l: (TransitivityEntry,TransitivityEntry,Fact)*):Unit = l.foreach(e=>add(e._1,e._2,e._3))

  def n(entry: TransitivityEntry,g:Option[Goal]=None) = {
    if (g.isDefined && entry.goal.isBelow(g.get))
      throw new IllegalArgumentException("entry is not above goal ")
    graph get entry
  }

  def getTermEntry(term:Term): TransitivityEntry = {
    graph.nodes.find(n=> n.tp==term).asInstanceOf[TransitivityEntry] //TODO check that this handles the case of terms at different goals
  }

  def compareAtGoal(a:TransitivityEntry,b:TransitivityEntry,g:Goal):Option[Boolean] = {
    if (n(a,Some(g)).withSubgraph(nodes = _.goal.isAbove(g),edges = isEdgeAboveGoal(_,g)).pathTo(n(b,Some(g))).isDefined){
      return Some(true)
    }
    None
  }

  def getProofAtGoal(x:Term,y:Term,g:Goal):Option[List[Fact]]={
    getProofAtGoal(getTermEntry(x),getTermEntry(y),g) //TODO check that this gets the right terms
  }

  def getProofAtGoal(x:TransitivityEntry,y:TransitivityEntry,g:Goal):Option[List[Fact]]={
    val path = n(x,Some(g)).withSubgraph(nodes = _.goal.isAbove(g),edges = isEdgeAboveGoal(_,g)).pathTo(n(y,Some(g)))
    if (path.isDefined) Some(path.map(_.edges).get.map(_.label).toList.asInstanceOf[List[Fact]])
    else None
  }


  override def toString:String = graph.toString()

  def getSubGraph(g:Goal):Graph[TransitivityEntry,LUnDiEdge] = {
    graph filter graph.having(node = _.goal.isAbove(g),edge = isEdgeAboveGoal(_,g))
  }


  type NT = graph.NodeT
  type ET = graph.EdgeT

  /** Determines if the goal of an edge is above the goal of a node*/
  private def isEdgeAboveGoal(e:ET,g:Goal)= {
    e.label match {
      case goal: Goal =>
        goal.isAbove(g)
      case _ => true
    }
  }


  def transClosureAtGoal(t1:TransitivityEntry,g:Goal) = {
    val subGraph = getSubGraph(g)
    (subGraph get t1).outerNodeTraverser.toSet
  }

  def dualTransClosureAtGoal(t1:TransitivityEntry,g:Goal)= {
    val subGraph = getSubGraph(g)
    (subGraph get t1).outerNodeTraverser.withDirection(Predecessors).toSet
  }


}

case class TransitiveGraph(globalName: GlobalName) extends RelationGraph(globalName) {
  override val directed=true
}

case class EqualityGraph(globalName: GlobalName) extends RelationGraph(globalName) {
  override val directed=false
}

