package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._

/** an object to extract dependencies from a controller */
object Relational {
  /** get all archives */
  def getArchives(controller: Controller): List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  /* Generates a dot-file for easier viewing of the dependency graph */
  def generateDot[A](name : String, f : (A => String), l : List[Set[A]], m : Map[A,Set[A]]) : Unit = {
    val w = new StringBuilder
    def writeln(s: String): Unit = w.append(s + "\n")
    writeln("digraph " + name + " {")

    var nodes : Set[String] = Set.empty
    def newnode(arg : A, important : Boolean = false) : Unit = {
      if (!nodes.contains(f(arg))) {
        val extra = if (important) "[shape=box style=filled fillcolor=red]" else "[shape=oval style=filled fillcolor=white]"
        writeln("    " + f(arg) + extra + ";")
        nodes += f(arg)
      }
    }

    def isImportant(d : A) : Boolean = d match {
        case FileBuildDependency(kk,_,_) => kk == "alltex"
        case _ => false
    }

    for (s <- l) {
      for (e <- s) {
        newnode(e,important = isImportant(e))
        for (tar <- m(e)) {
          newnode(tar, important = isImportant(tar))
          writeln("    " + f(e) + " -> " + f(tar) + ";")
        }
      }
    }

    writeln("}")
    reflect.io.File(name + ".dot").writeAll(w.result())
  }

  def dependencyNodeName[A](d : A) : String = d match {
    case FileBuildDependency(k,a,i) => "\"" + i + "\n(" + k + ")\""
    case PhysicalDependency(f) => "\"" + f.toFilePath.tail.tail.tail.tail.tail.toString + "\""
    case l => println("Error: Dependency not dotted: " + l); "other"
  }

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

    val sorted = topsort(controller, m)
    generateDot("topsorted",dependencyNodeName[A],sorted,m)
    sorted.flatMap(_.toList.sortBy(d => depclosuresize(d)))
  }
}
