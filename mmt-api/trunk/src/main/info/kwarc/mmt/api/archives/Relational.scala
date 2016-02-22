package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._
import parser._
import utils._
import documents._

/** a build target for computing mmt structure dependencies */
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
    val inPathOMDoc = bt.inPath.toFile.setExtension("omdoc").toFilePath
    DPath(bt.base / inPathOMDoc.segments)
  }

  def buildFile(bf: BuildTask): BuildResult = {
    val ps = new ParsingStream(bf.base / bf.inPath.segments, IsRootDoc(getDocDPath(bf)),
      bf.archive.namespaceMap, "mmt", File.Reader(bf.inFile))
    val doc = parser(ps)(bf.errorCont).asInstanceOf[Document] // must succeed because of IsRootDoc
    storeRel(doc)
    doc.getModulesResolved(controller.localLookup) foreach indexModule
    BuildResult.empty
  }

  override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): BuildResult = {
    /* here we clean memory to avoid conflicts with subsequent builds.
     * without it nat.mmt results in several "error: invalid unit:" */
    controller.memory.content.clear
    // TODO: avoid memory usage and add dependencies (to be computed) directly in StructureParser
    BuildResult.empty
  }

  /** extract and store the relational information about a knowledge item */
  private def storeRel(se: StructuralElement) {
    controller.relman.extract(se) { r =>
      controller.depstore += r
    }
  }

  /** index a module */
  private def indexModule(mod: StructuralElement) {
    storeRel(mod)
  }
}

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
