package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.Module
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
  val outDim = relational

  val parser = new KeywordBasedParser(DefaultObjectParser)

  override val outExt = "rel"

  override def start(_args: List[String]) {
    controller.extman.addExtension(parser)
  }

  def includeFile(s: String) = s.endsWith(".mmt")

  def buildFile(bf: BuildTask): Unit = {
    val inPathOMDoc = bf.inPath.toFile.setExtension("omdoc").filepath
    val dpath = DPath(bf.base / inPathOMDoc.segments) // bf.narrationDPath except for extension
    val ps = new ParsingStream(bf.base / bf.inPath.segments, dpath, bf.archive.namespaceMap, "mmt", File.Reader(bf.inFile))
    val doc = parser(ps)(bf.errorCont)
    writeToRel(doc, bf.archive / relational / bf.inPath)
    doc.getModulesResolved(controller.globalLookup) foreach { mod => indexModule(bf.archive, mod) }
    val rs: RelStore = controller.depstore
    val deps = rs.querySet(doc.path, +Declares * HasType(IsView) * +HasCodomain)
    println(deps)

  }

  /** extract and write the relational information about a knowledge item */
  private def writeToRel(se: StructuralElement, file: File): Unit = {
    val relFile = file.setExtension("rel")
    log("[  -> relational]     " + relFile.getPath)
    val relFileHandle = File.Writer(relFile)
    controller.relman.extract(se) { r =>
      relFileHandle.write(r.toPath + "\n")
      controller.depstore += r
    }
    relFileHandle.close()
  }

  /** index a module */
  private def indexModule(a: Archive, mod: Module): Unit = {
    // write relational file
    writeToRel(mod, a / relational / Archive.MMTPathToContentPath(mod.path))
  }
}
