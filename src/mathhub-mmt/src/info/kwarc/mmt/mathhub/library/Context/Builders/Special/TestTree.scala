package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.frontend.Controller

class TestTree(
                controller: Controller,
                mathhub: MathHub
              ) extends OpaqueTree(controller, mathhub) {
  val key = "test"
  val displayName = "Test Tree Elements"

  /** applicable to every archive */
  def applicable(archive: String): Boolean = true

  /** only the root node exists */
  def children(archive: String, path: List[String]): List[String] = Nil

  /** every node has content */
  def hasContent(archive: String, path: List[String]): Boolean = true

  /** a single piece of content is found in the node */
  def content(archive: String, path: List[String]): List[String] = List("dummy")

  /** contains a single dummy node */
  def opaqueContent(archive: String, path: List[String], name: String): (String, String) = {
    ("html", "This is a <b>test</b> tree inserted into the root")
  }
}