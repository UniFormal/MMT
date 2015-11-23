package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.notations.{TextNotation, NotationContainer}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{ComplexStep, NamespaceMap, LocalName, GlobalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.symbols._

/** A class representing a refactoring heuristic for a set of theories */


abstract class RefactoringHeuristic {
  protected implicit val controller : Controller

  protected var theories : Set[DeclaredTheory] = Set()
  protected var donetheories : Set[DeclaredTheory] = Set()

  protected def thadd(th:DeclaredTheory) = {
    var tOpt = theories.find(t => t.path==th.path)
    if (tOpt.isDefined) theories= theories-tOpt.get
    tOpt = donetheories.find(t => t.path==th.path)
    if (tOpt.isDefined) donetheories= donetheories-tOpt.get
    theories+=th
    controller.library.delete(th.path)
    controller.library.add(th)
  }

  protected def isDone(th:DeclaredTheory) = {
    theories = theories-th
    donetheories+=th
  }

  def run

  def apply(ths:List[DeclaredTheory]) : List[DeclaredTheory]
  = apply(ths.toSet).toList

  def apply(ths:Set[DeclaredTheory]) : Set[DeclaredTheory] = {
    theories = ths
    while (theories.nonEmpty) run
    donetheories
  }

}

/** intersects every T along the morphism T->S with the largest domain */

case class NaiveHeuristic(ctrl : Controller) extends RefactoringHeuristic {
  implicit val controller = ctrl
  lazy val finder = AxiomaticViewfinder(controller)
  lazy val intersecter = new Intersecter {
    implicit val controller : Controller = ctrl
  }
  var i = -1
  finder.setParameters(true,true,false,true,0)

  def run {
    TheorySet.reset
    val th = theories.head
    val views = theories.tail.collectFirst{case t if finder.findBest(th,t).isDefined => finder.findBest(th,t).get}//map(t => (t,finder.findBest(th,t))).collect{ case (t,Some(v)) => (t,v)}
    if (views.isEmpty) isDone(th) else {
      val top = views.get
      top.distribute
      println(th.path)
      println(top)
      // val takecodname = top.evaluateOnCod>=top.evaluateOnDom
      val t = intersecter.intersect(top)
      println(t._1)
      println(t._2)
      (t._1::t._2::t._1.allincludes:::t._2.allincludes).distinct.foreach(ts => {
        if(ts.getPath.isEmpty) ts.setPath(th.parent ? LocalName({i+=1;"INT_"+i}))
        if(ts.meta.isEmpty && t._1.meta.isDefined && t._1.meta==t._2.meta) ts.setMeta(t._1.meta.get)
      })
      val res = (t._1.toTheory:::t._2.toTheory).distinct
      println(res)
      res.foreach(m => thadd(m))
    }


  }

}
