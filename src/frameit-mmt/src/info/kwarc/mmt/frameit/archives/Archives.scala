package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.api._

object Archives {

  object FrameWorld {
    object ScrollKeys {
      private val _scrollMeta: MPath = FrameWorld.rootDocument ? "ScrollMeta"

      val name: GlobalName = _scrollMeta ? "name"
      val problemTheory: GlobalName = _scrollMeta ? "problemTheory"
      val solutionTheory: GlobalName = _scrollMeta ? "solutionTheory"
      val description: GlobalName = _scrollMeta ? "description"
    }

    val archiveID: String = "FrameIT/frameworld"
    val rootDocument: DPath = DPath(URI("http://mathhub.info/FrameIT/frameworld"))
    val FactCollection: MPath = Path.parseM("http://mathhub.info/FrameIT/frameworld?FactCollection", NamespaceMap.empty)
  }

  def getPaths(rootDir: File): List[File] = {
    val mathhubRoot = rootDir / "MathHub"
    List(
      mathhubRoot / "MMT" / "urtheories",
      mathhubRoot / "MMT" / "LFX",
      mathhubRoot / "MitM" / "core",
      mathhubRoot / "MitM" / "Foundation",
      mathhubRoot / "FrameIT" / "frameworld"
    )
  }

  object MetaSymbols {
    val LFtype: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?Typed?type", NamespaceMap.empty)
    val jder: GlobalName = Path.parseS("http://mathhub.info/MitM/Foundation?Logic?ded", NamespaceMap.empty)
    val jdoteq: GlobalName = Path.parseS("http://mathhub.info/MitM/Foundation?Logic?eq", NamespaceMap.empty)
    val proofSketch: GlobalName = Path.parseS("http://mathhub.info/MitM/Foundation?InformalProofs?proofsketch", NamespaceMap.empty)
  }
}
