package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.utils._

/** a build target for computing structure dependencies */
class Relational extends TraversingBuildTarget {
  /** source by default, may be overridden */
  def inDim = source

  def key = "mmt-deps"

  /** relational */
  val outDim = source

  val parser = new RelKeywordBasedParser

  override val outExt = "mmt"

  override def start(_args: List[String]) {
    controller.extman.addExtension(parser)
  }

  def includeFile(s: String) = s.endsWith(".mmt")

  private def getDocDPath(bt: BuildTask): DPath = {
    val inPathOMDoc = bt.inPath.toFile.setExtension("omdoc").filepath
    DPath(bt.base / inPathOMDoc.segments)
  }

  def buildFile(bf: BuildTask): Unit = {
    val ps = new ParsingStream(bf.base / bf.inPath.segments, getDocDPath(bf), bf.archive.namespaceMap, "mmt", File.Reader(bf.inFile))
    val doc = parser(ps)(bf.errorCont)
    storeRel(doc)
    doc.getModulesResolved(controller.localLookup) foreach indexModule
  }

  override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): Unit = {
    val rs: RelStore = controller.depstore
    builtChildren.foreach { bt =>
      if (!bt.isDir) {
        val d = getDocDPath(bt)
        println(docPathToFilePath(d).mkString)
        val usedTheories = rs.querySet(d, +Declares * RelationExp.Deps)
        val reducedTheories = usedTheories.map {
          case MPath(p, LocalName(hd :: _ :: _)) => MPath(p, LocalName(List(hd)))
          case t => t
        }.toList.sortBy(_.toString)
        reducedTheories.foreach { theo =>
          val provider = rs.querySet(theo, -Declares).toList.sortBy(_.toString)
          if (provider.isEmpty)
            println("  nothing provides: " + theo)
          else {
            val ds = provider.flatMap(docPathToFilePath)
            ds match {
              case Nil => println("  no document found for: " + theo)
              case hd :: Nil =>
                if (ds == docPathToFilePath(d))
                  println("  theory provided in same document: " + theo)
                else
                  println("  " + hd + " for " + theo)
              case _ =>
                println("  several documents found for: " + theo)
                println("    " + ds.mkString(" "))
            }
          }
        }
      }
    }
  }

  def docPathToFilePath(p: Path): List[File] = p match {
    case DPath(uri) =>
      controller.backend.getStores.filter(_.isInstanceOf[Archive]).
        flatMap { s =>
          val a = s.asInstanceOf[Archive]
          if (a.narrationBase <= uri) {
            val b = a.narrationBase.toString
            val c = uri.toString.stripPrefix(b)
            if (c.startsWith("/http..")) None // filter out content documents
            else Some(File(a.rootString + "/source" + c).setExtension("mmt"))
          } else None
        }.map { f =>
        if (f.length() == 0) log("missing file: " + f + " for path " + p)
        f
      }
    case _ => Nil
  }

  /** extract and store the relational information about a knowledge item */
  private def storeRel(se: StructuralElement): Unit = {
    controller.relman.extract(se) { r =>
      controller.depstore += r
    }
  }

  /** index a module */
  private def indexModule(mod: StructuralElement): Unit = {
    storeRel(mod)
  }
}

class ArchiveDeps extends Extension {

  def buildAllDeps(): Unit = {
    val builder = new Relational
    builder.init(controller)
    builder.start(Nil)
    // val saveStore = controller.depstore
    // controller.depstore = new RelStore(controller.report)
    // TODO save and restore depstore
    // maybe sort archives in dependency order via "dependencies" property
    Relational.getArchives(controller).foreach { a =>
      builder(Build, a, EmptyPath)
    }
    // extract deps and return them
  }

  override def start(args: List[String]) {
    buildAllDeps()
  }
}

object Relational {
  // see getDocDPath
  def getDPath(a: Archive, fp: FilePath): DPath = {
    val inPathOMDoc = fp.toFile.setExtension("omdoc").filepath
    DPath(a.narrationBase / inPathOMDoc.segments)
  }

  def getArchives(controller: Controller): List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  def report(controller: Controller, s: String): Unit =
    controller.report("deps", s)

  // see docPathToFilePath
  def dPathToFile(controller: Controller)(p: Path): List[(Archive, FilePath)] =
    p match {
      case DPath(uri) =>
        getArchives(controller).
          flatMap { a =>
            if (a.narrationBase <= uri) {
              val b = a.narrationBase.toString
              val c = uri.toString.stripPrefix(b)
              if (c.startsWith("/http..")) None // filter out content documents
              else Some((a, File(a.rootString + "/source" + c).setExtension("mmt")))
            } else None
          }.map { afp =>
          if (afp._2.length() == 0) report(controller, "missing file: " + afp._2 + " for path " + p)
          (afp._1, FilePath((afp._1.root / "source").relativize(afp._2).segments.tail))
        }
      case _ => Nil
    }

  def getSingleDeps(controller: Controller, a: Archive, fp: FilePath): Set[(Archive, FilePath)] = {
    val rs = controller.depstore
    val d = getDPath(a, fp)
    val usedTheories = rs.querySet(d, +Declares * RelationExp.Deps)
    val reducedTheories = usedTheories.map {
      case MPath(p, LocalName(hd :: _ :: _)) => MPath(p, LocalName(List(hd)))
      case t => t
    }.toList.sortBy(_.toString)
    var result: Set[(Archive, FilePath)] = Set.empty
    reducedTheories.foreach { theo =>
      val provider = rs.querySet(theo, -Declares).toList.sortBy(_.toString)
      if (provider.isEmpty)
        report(controller, "  nothing provides: " + theo + " for " + d)
      else {
        val ds = provider.flatMap(dPathToFile(controller))
        result ++= ds
        ds match {
          case Nil => report(controller, "  no document found for: " + theo)
          case hd :: Nil =>
            if (ds == dPathToFile(controller)(d))
              report(controller, "  theory provided in same document: " + theo)
            else
              report(controller, "  " + hd + " for " + theo)
          case _ =>
            report(controller, "  several documents found for: " + theo)
            report(controller, "    " + ds.mkString(" "))
        }
      }
    }
    result -= ((a, fp))
    println(result)
    result
  }

  def topsort[A](m: Map[A, Set[A]]): List[Set[A]] = {
    if (m.isEmpty) Nil
    else {
      val (noDeps, rest) = m.partition(_._2.isEmpty)
      if (noDeps.isEmpty) List(Set.empty, m.keySet) // oops cycles !!!
      else {
        val fst = noDeps.keySet
        fst :: topsort(rest.map(p => (p._1, p._2.diff(fst))))
      }
    }
  }

  def getDeps(controller: Controller, args: Set[(Archive, FilePath)]): List[Set[(Archive, FilePath)]] = {
    var visited: Set[(Archive, FilePath)] = Set.empty
    var unknown = args
    var deps: Map[(Archive, FilePath), Set[(Archive, FilePath)]] = Map.empty
    while (unknown.nonEmpty) {
      val p = unknown.head
      val ds = getSingleDeps(controller, p._1, p._2)
      deps += ((p, ds))
      visited += p
      unknown -= p
      unknown ++= ds.diff(visited)
    }
    topsort(deps)
  }
}
