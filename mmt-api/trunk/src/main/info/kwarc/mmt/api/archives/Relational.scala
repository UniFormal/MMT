package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.utils._

/**
 * a build target for computing structure dependencies
 *
 */
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
        usedTheories.foreach { t =>
          val provider = rs.querySet(t, -Declares)
          if (provider.isEmpty)
            println("  nothing provides: " + t)
          else {
            val ds = provider.flatMap(docPathToFilePath).toList
            ds match {
              case Nil => println("  no document found for: " + t)
              case hd :: Nil =>
                if (ds == docPathToFilePath(d))
                  println("  theory provided in same document: " + t)
                else
                  println("  " + hd)
              case _ =>
                println("  several documents found for: " + t)
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
          if (a.narrationBase <= uri) Some(a) else None
        }.map { a =>
        val b = a.narrationBase.toString
        val f = File(a.rootString + "/source" + uri.toString.stripPrefix(b)).setExtension("mmt")
        if (f.length() == 0) log("missing file: " + f)
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
