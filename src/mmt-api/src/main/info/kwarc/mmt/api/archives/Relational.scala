package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._

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

  /* Generates a dot-file for easier viewing of the dependency graph */
  def generateDot[A](name : String, f : (A => String), m : Map[A,Set[A]]) : Unit = {
    val w = new StringBuilder
    def writeln(s: String): Unit = w.append(s + "\n")
    writeln("digraph " + name + " {")

    var nodes : Set[String] = Set.empty

    def newnode(arg : A) : Unit = {
      if (!nodes.contains(f(arg))) {
        writeln("    " + f(arg) + ";")
        nodes += f(arg)
      }
    }

    for ((k,v) <- m) {
      newnode(k)
      for (tar <- v) {
        newnode(tar)
        writeln("    " + f(k) + " -> " + f(tar) + ";")
      }
    }

    writeln("}")
    reflect.io.File(name + ".dot").writeAll(w.result())
  }

  def dependencyNodeName[A](d : A) : String = d match {
    case FileBuildDependency(k,a,i) => "\"" +  i + "\n" + a.hashCode() + "\n(" + k + ")\""
    case PhysicalDependency(f) => "\"" + f.toString + "\""
    case l => println("Error: Dependency not dotted: " + l); "other"
  }

  def topsort[A](controller: Controller, m: Map[A, Set[A]]): List[Set[A]] = {
    if (m.isEmpty) Nil
    else {
      val (noDeps, rest) = m.partition(_._2.isEmpty)
      if (noDeps.isEmpty) {
        generateDot(name = "cyclic_remainder", dependencyNodeName[A], m)
        controller.report(new Error(shortMsg = "cyclic deps (see \"cyclic_remainder.dot\"): " + m) {})
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

  def newFlatTopsort[A](controller: Controller, m: Map[A, Set[A]]): List[A] = {
    def depclosuresize(start : A) : Int = {
      if (m.keySet.contains(start)) {
        var sz = 1
        for (k <- m(start)) {
          sz += depclosuresize(k)
        }
        sz
      } else { 0 }
    }

    topsort(controller, m).flatMap(_.toList.sortBy(d => depclosuresize(d)))
  }
}
