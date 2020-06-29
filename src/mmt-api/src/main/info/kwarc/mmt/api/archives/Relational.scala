package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._

/** an object to extract dependencies from a controller */
object Relational {
  /** get all archives */
  def getArchives(controller: Controller): List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  def topsort[A](controller: Controller, m: Map[A, Set[A]]): List[Set[A]] = {
    if (m.isEmpty) Nil
    else {
      val (noDeps, rest) = m.partition(_._2.isEmpty)
      if (noDeps.isEmpty) {
        controller.report(new Error(shortMsg = "cyclic dependencies: " + m) {})
        List(Set.empty, m.keySet)
      }
      else {
        val fst : Set[A] = noDeps.keySet
        fst :: topsort(controller, rest.map(p => (p._1, p._2.diff(fst))))
      }
    }
  }

  def flatTopsort[A](controller: Controller, m: Map[A, Set[A]]): List[A] = {
    topsort(controller, m).flatMap(_.toList.sortBy(_.toString))
  }
}
