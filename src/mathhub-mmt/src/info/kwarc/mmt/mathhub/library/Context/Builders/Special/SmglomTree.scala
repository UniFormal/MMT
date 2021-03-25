package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.mathhub.library.{IHubReference, ISourceReference}

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
    ("application/xhtml+stex", readModulePartHTML(archive, path.head, path(1)))
  }
}

trait STeXReader { this: SmglomTree =>

  val extension = "xhtml"

  private def listModuleFiles(archive: String): List[String] = {
    val root = controller.backend.getArchive(archive).getOrElse(return Nil).root / extension
    if (root.exists && root.isDirectory) {
      root.listFiles(_.isFile).toList.map(_.getName)
        .filter(_.endsWith("." + extension))
        .map(_.dropRight(extension.length + 1))
        .sorted
    } else {
      Nil
    }
  }

  protected def findModules(archive: String): List[String] = {
    // if a name has an extra '.' it is part of that module!
    listModuleFiles(archive).filter(p => !p.contains('.'))
  }

  protected def readModuleName(archive: String, module: String): String = {
    module
  }

  protected def findModuleParts(archive: String, module: String): List[String] = {
    listModuleFiles(archive)
      .filter(f => f.startsWith(module + ".") || f == module)
      .map(f => if (f == module) "" else f.drop(module.length + 1)) // drop the module prefix
  }

  def sourceRef(archive: IHubReference, path: List[String]): Option[ISourceReference] = {
    val (module, part) = path match {
      case List(module) => (module, "")
      case List(module, part) => (module, part)
      case _ => return None
    }
    val filename = part match {
      case "" => module + "." + extension
      case s => module + "." + s + "." + extension
    }

    Some(ISourceReference(archive, None,  Some(extension + "/" + filename)))
  }

  private def loadXML(archive: String, module: String, part: String): Option[scala.xml.Elem] = {
    val root = controller.backend.getArchive(archive).getOrElse(return None).root / extension
    val filename = part match {
      case "" => module + "." + extension
      case s => module + "." + s + "." + extension
    }
    Some(scala.xml.XML.loadFile(root / filename))
  }

  protected def readModulePartName(archive: String, module: String, part: String): String = {
    val kind = if (part == "") { "Signature" } else { "Language " + part }
    val title = readModulePartNameTitle(archive, module, part).map(List(_)).getOrElse(Nil)

    (module :: kind :: title).mkString(" | ")
  }

  private def readModulePartNameTitle(archive: String, module: String, part: String): Option[String] = {
    val documentNode = loadXML(archive, module, part).getOrElse(return None)
    val titleNode = findNodeByTag(documentNode, "title").getOrElse(return None)
    val title = titleNode.text
    if (title != "") Some(title) else None
  }

  protected def readModulePartHTML(archive: String, module: String, part: String): String = {
    val documentNode = loadXML(archive, module, part).getOrElse(return "")
    val contentNodes = findNodeByAttr(documentNode, "class", "ltx_page_main")
    val content = contentNodes.getOrElse(return "No content found in document")
    content.toString
  }

  private def findNodeByTag(element: scala.xml.Elem, tag: String): Option[scala.xml.Node] = {
    element \\ tag headOption
  }
  private def findNodeByAttr(element: scala.xml.Elem, attribute: String, value: String): Option[scala.xml.Node] = {
    def attrEqual(n: scala.xml.Node, a: String, v: String) =  (n \ ("@" + a)).text == value
    element \\ "_" find { n => attrEqual(n, attribute, value) }
  }
}