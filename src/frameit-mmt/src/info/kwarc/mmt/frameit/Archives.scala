package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.utils.File

object Archives {

  val frameworldIdentifier = "FrameIT/frameworld"

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
}
