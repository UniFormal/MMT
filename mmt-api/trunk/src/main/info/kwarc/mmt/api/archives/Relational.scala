package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
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
  val outDim = narration

  val parser = new KeywordBasedParser(DefaultObjectParser)

  override val outExt = "omdoc"

  override def start(_args: List[String]) {
    controller.extman.addExtension(parser)
  }

  def includeFile(s: String) = s.endsWith(".mmt")

  def buildFile(bf: BuildTask): Unit = {
    val inPathOMDoc = bf.inPath.toFile.setExtension("omdoc").filepath
    val dpath = DPath(bf.base / inPathOMDoc.segments) // bf.narrationDPath except for extension
    val ps = new ParsingStream(bf.base / bf.inPath.segments, dpath, bf.archive.namespaceMap, "mmt", File.Reader(bf.inFile))
    val doc = parser(ps)(bf.errorCont)
    val narrFile = getOutFile(bf.archive, bf.inPath)
    if (!narrFile.exists) narrFile.createNewFile() // these files are needed
    storeRel(doc)
    doc.getModulesResolved(controller.localLookup) foreach indexModule
  }

  override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): Unit = {
    bd.outFile.up.mkdirs
    val doc = controller.get(DPath(bd.archive.narrationBase / bd.inPath.segments)).asInstanceOf[Document]
    storeRel(doc)
    val rs: RelStore = controller.depstore
    val docs = rs.querySet(doc.path, +Declares * HasType(IsDocument))
    docs.foreach { d =>
      val s = rs.querySet(d, +Declares * RelationExp.Deps * -Declares * HasType(IsDocument))
      println(docPathToFilePath(d).mkString(" ") + ": " + (s - d).flatMap(docPathToFilePath).mkString(" "))
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
