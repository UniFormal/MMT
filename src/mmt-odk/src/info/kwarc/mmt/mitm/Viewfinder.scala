package info.kwarc.mmt.mitm

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.archives.{Archive, BuildTarget, Update}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.refactoring.{AlignmentFinder, FindingProcess, Hasher, HashesNormal}
import info.kwarc.mmt.api.utils.FilePath

import scala.collection.mutable

class Viewfinder extends BuildTarget{
  val key = "viewfinding"

  lazy val alignmentFinder : AlignmentFinder = controller.extman.get(classOf[AlignmentFinder]).headOption.getOrElse {
    val af = new AlignmentFinder
    controller.extman.addExtension(af)
    af
  }

  lazy val mitm : Archive = controller.backend.getArchive("MitM/smglom").getOrElse(???)

  override def clean(a: Archive, in: FilePath): Unit = ???

  override def build(a: Archive, up: Update, in: FilePath): Unit = ???

  def find(mp : MPath, to : Archive) = {
    val hasher = default(mp, to) // TODO
    val proc = new FindingProcess(this.report,hasher)
    proc.run(from = List(hasher.get(mp).get))
  }

  private def default(mp : MPath,to : Archive) = {
    val hasher = alignmentFinder.getHasher
    hasher.cfg.setDoDefs(false)
    hasher.cfg.setMultithreaded(false)
    alignmentFinder.addArchives(mitm,to,hasher)
    val news = alignmentFinder.getFlat(List(mp)).filterNot(p => mitm.allContent.contains(p.path))
    news foreach (t => hasher.add(t,Hasher.FROM))
    hasher
  }

}
