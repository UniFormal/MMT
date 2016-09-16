package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{GlobalName, MPath}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}

import scala.collection.mutable

abstract class ClusteringFactory[NodeState,EdgeState](controller: Controller, initNode : NodeState,initEdge : EdgeState) {
  def strategy(c : Constant) : Unit
  def evaluation(e : Edge) : Double
  val nodes : scala.collection.mutable.HashMap[GlobalName, Node] = mutable.HashMap.empty
  val edges : scala.collection.mutable.HashMap[(GlobalName,GlobalName), Edge] = mutable.HashMap.empty

  protected case class Node(c : Constant) {
    def getNeighbors : List[Node] = getEdges.map(e => if (e.from == this) e.to else e.from).distinct
    def getEdges = edges.collect{case ((f : GlobalName,t : GlobalName),e : Edge) if f == c.path || t == c.path
      => e}.toList
    var state : NodeState = initNode
    def deps = ((if (c.tp.isDefined) ArchiveStore.getSymbols(c.tp.get) else Nil) :::
      (if (c.df.isDefined) ArchiveStore.getSymbols(c.df.get) else Nil)).distinct.filter(_.module == c.parent)
  }
  protected case class Edge(fromName : GlobalName, toName : GlobalName) {
    protected def getNode(n : GlobalName) = GetNode(n)
    var state : EdgeState = initEdge
    def getValue = evaluation(this)
    val from = getNode(fromName)
    val to = getNode(toName)
  }

  protected def GetNode(c : Constant) = nodes.getOrElse(c.path,NewNode(c))
  protected def GetNode(n : GlobalName) = nodes.getOrElse(n,{
    NewNode(controller.library.getConstant(n))
  })
  private def NewNode(c : Constant) = {
    val newone = Node(c)
    nodes(c.path) = newone
    newone
  }
  protected def GetEdge(n : GlobalName, m : GlobalName) = edges.getOrElse((n,m),edges.getOrElse((m,n),{
    val newone = Edge(n,m)
    edges((n,m)) = newone
    newone
  }))
  def run(t : DeclaredTheory) = t.getDeclarations.collect{case c : Constant => c}.foreach(strategy)
}

class StandardCluster(controller : Controller,theory : DeclaredTheory)
  extends ClusteringFactory[Int,Int](controller,0,0) {

  def strategy(c : Constant) = {
    val syms = GetNode(c).deps
    for (i <- syms.indices) {
      GetEdge(syms(i),c.path).state += 1
      for (j <- 0 until i) GetEdge(syms(i),syms(j)).state += 1
    }
  }

  def evaluation(e : Edge) = {
    val from = e.state.toDouble / e.from.getEdges.map(_.state).sum.toDouble
    val to = e.state.toDouble / e.to.getEdges.map(_.state).sum.toDouble
    (from + to) / 2
  }
}