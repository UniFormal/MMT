package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._
import parser._
import utils._
import documents._

/** an object to extract dependencies from a controller */
object Relational {
  /** get all archives */
  def getArchives(controller: Controller): List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  def close[A](inMap: Map[A, Set[A]]): Map[A, Set[A]] = {
    var changed = true
    var m = inMap
    while (changed) {
      changed = false
      m = m.map(p => (p._1, {
        val n: Set[A] = p._2.flatMap(m).union(p._2)
        if (n.size != p._2.size) changed = true
        n
      }))
    }
    m.filter(p => p._2.contains(p._1))
  }

  def topsort[A](controller: Controller, m: Map[A, Set[A]]): List[Set[A]] = {
    if (m.isEmpty) Nil
    else {
      val (noDeps, rest) = m.partition(_._2.isEmpty)
      if (noDeps.isEmpty) {
        controller.report(new Error("cyclic deps: " + close(m)) {})
        List(Set.empty, m.keySet)
      }
      else {
        val fst = noDeps.keySet
        fst :: topsort(controller, rest.map(p => (p._1, p._2.diff(fst))))
      }
    }
  }

  def flatTopsort[A](controller: Controller, m: Map[A, Set[A]]): List[A] =
    topsort(controller, m).flatMap(_.toList.sortBy(_.toString))
}
