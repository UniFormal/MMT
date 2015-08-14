package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
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

  def getArchives: List[Archive] =
    controller.backend.getStores.collect { case a: Archive => a }

  // maybe sort archives in dependency order via "dependencies" property

  def buildAllDeps(): Unit = {
    val builder = new Relational
    builder.init(controller)
    builder.start(Nil)
    // val saveStore = controller.depstore
    // controller.depstore = new RelStore(controller.report)
    // TODO save and restore depstore
    getArchives.foreach { a =>
      builder(Build, a, EmptyPath)
    }
    // extract deps and return them
  }

  override def start(args: List[String]) {
    buildAllDeps()
  }
}
