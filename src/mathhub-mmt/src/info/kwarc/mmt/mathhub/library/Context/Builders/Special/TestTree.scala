package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.frontend.Controller

class TestTree(
                override protected val controller: Controller,
                override protected val mathhub: MathHub
              ) extends VirtualTree(controller, mathhub) {
  val key = "test"
  val displayName = "Test Tree Elements"

  def applicable(archive: String): Boolean = true // applicable to every archive

  def exists(archive: String, path: List[String]): Boolean = path.isEmpty // only a top-level node exists
  def children(archive: String, path: List[String]): List[String] = Nil // no children

  def isLeaf(archiveID: String, path: List[String]): Boolean = true

  def getLeafContent(archiveID: String, path: List[String]): (String, String) = {
    ("html", "This is a <b>test</b> tree inserted into the root")
  }
}