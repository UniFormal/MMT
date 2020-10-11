package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._

/** an object to extract dependencies from a controller */
object Relational {
  /** get all archives */
  def getArchives(controller: Controller): List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  def topsort[A](controller: Controller, m: Map[A, Set[A]]) : Option[List[Set[A]]] = {
    if (m.isEmpty) Some(Nil)
    else {
      val (noDeps, rest) = m.partition(_._2.isEmpty)
      if (noDeps.isEmpty) {
        controller.report(new Error(shortMsg = "cyclic dependencies: " + m) {})
        Dot.generateDot(name = "cyclical_dependencies", Dot.dependencyNodeName[A],List(rest.keySet),m)
        None
      }
      else {
        val fst : Set[A] = noDeps.keySet
        topsort(controller, rest.map(p => (p._1, p._2.diff(fst)))).map(olsa => fst :: olsa)
      }
    }
  }

  @deprecated
  def flatTopsort[A](controller: Controller, m: Map[A, Set[A]]): Option[List[A]] = {
    val ts = topsort(controller, m)
    if (ts.isDefined) {
      Some(ts.get.flatMap(_.toList.sortBy(_.toString)))
    } else { None }
  }

  def newFlatTopsort[A](controller: Controller, m: Map[A, Set[A]]): Option[List[A]] = {
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
    if (sorted.isEmpty) { return None }

    Dot.generateCondensedDot( name = "topsorted_alltex_graph", Dot.simpleDependencyNodeName[A], sorted.get.flatten.toSet, m)
    Dot.generateDot(name = "topsorted", Dot.dependencyNodeName[A], sorted.get, m)

    Some(sorted.get.flatMap(_.toList.sortBy(d => depclosuresize(d))))
  }
}

object Dot {

  /* Generates a dot-file for easier viewing of the dependency graph */
  def generateDot[A](name : String, f : (A => String), l : List[Set[A]], m : Map[A,Set[A]]) : Unit = {
    val w = new StringBuilder
    def writeln(s: String): Unit = w.append(s + "\n")
    writeln("digraph " + name + " {")

    var nodes : Set[String] = Set.empty
    def newnode(arg : A, important : Boolean = false) : Unit = {
      if (!nodes.contains(f(arg))) {
        val extra = if (important) "[shape=box style=filled fillcolor=gold]" else "[shape=oval style=filled fillcolor=white]"
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

  /* generates only dependency graph between alltex files */
  def generateCondensedDot[A](name : String, f : (A => String), l : Set[A], m : Map[A,Set[A]]) : Unit = {
    val w = new StringBuilder
    def writeln(s: String): Unit = w.append(s + "\n")
    writeln("digraph " + name + " {")

    var nodes : Set[String] = Set.empty
    for (e <- l) {
      e match {
        case FileBuildDependency("alltex",_,_) =>
          writeln("    " + f(e) + ";")
          nodes += f(e)
        case _ =>
      }
    }

    def inDependencyClosure(start : String, end : String) : Boolean = {
      var closure : Set[String]  = Set.empty
      var expand  : Boolean = true

      while (expand) {
        var newClosure : Set[String] = Set(start)
        for (c <- closure) {
          for (k <- m.keySet) {
            if (f(k) == c) {
              newClosure = newClosure.union(m(k).map(f))
            }
          }
        }
        expand = newClosure.size > closure.size
        closure = newClosure
      }

      closure.contains(end)
    }

    for (at <- nodes) {
      for (tar <- nodes - at) {
        if (inDependencyClosure(at, tar)) {
          writeln("    " + at + " -> " + tar + ";")
        }
      }
    }

    writeln("}")
    reflect.io.File(name + ".dot").writeAll(w.result())
  }

  def dependencyNodeName[A](d : A) : String = d match {
      case FileBuildDependency(k,a,i) => "\"" + i + "\n(" + k + ")\""
      case PhysicalDependency(f) => "\"" + f.toFilePath.tail.tail.tail.tail.tail.mkString("/") + "\""
      case l => "other"
  }

  def simpleDependencyNodeName[A](d : A) : String = d match {
    case FileBuildDependency(_,_,i) => "\"" + i + "\""
    case l => "other"
  }
}
