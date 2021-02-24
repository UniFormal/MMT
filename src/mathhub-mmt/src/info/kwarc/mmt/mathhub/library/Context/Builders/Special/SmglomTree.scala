package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.{Archive, MathHub}
import info.kwarc.mmt.api.frontend.Controller

class SmglomTree(
                controller: Controller,
                mathhub: MathHub
              ) extends OpaqueTree(controller, mathhub) with STeXReader {
  val key = "smglom-stex"
  val displayName = "STeX Content"

  /** applicable to smglom archives */
  def applicable(archive: String): Boolean = {
    archive.startsWith("smglom/")
  }

  /** only the root node exists */
  def children(archive: String, path: List[String]): List[String] = path match {
    case Nil => findModules(archive)
    case mod::Nil => findModuleParts(archive, mod)
    case _ => Nil
  }

  /** module parts have content */
  def hasContent(archive: String, path: List[String]): Boolean = path.length == 2

  override def displayName(archive: String, path: List[String]): String = path match {
    case Nil => displayName
    case module :: Nil => readModuleName(archive, module)
    case module :: part :: Nil => readModulePartName(archive, module, part)
    case _ => ""
  }

  /* shtml found in the node */
  def content(archive: String, path: List[String]): List[String] = List("shtml")

  /** contains a single dummy node */
  def opaqueContent(archive: String, path: List[String], name: String): (String, String) = {
    ("html", readModulePartHTML(archive, path.head, path(1)))
  }
}

trait STeXReader { this: SmglomTree =>

  protected def findModules(archive: String): List[String] = {
    List("fake module")
  }

  protected def readModuleName(archive: String, module: String): String = {
    module
  }

  protected def findModuleParts(archive: String, module: String): List[String] = {
    List("", "en", "de")
  }

  protected def readModulePartName(archive: String, module: String, part: String): String = {
    if(part == "") {
      "Signature"
    } else {
      "Language " + part
    }
  }

  protected def readModulePartHTML(archive: String, module: String, part: String): String = {
    "Bla bla bla, this will read html <b>" + part + "</b>"
  }
}