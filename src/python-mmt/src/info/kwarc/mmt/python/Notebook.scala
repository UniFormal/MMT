package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import utils._

import JSONConversions._

case class Notebook(languageInfo: LanguageInfo, metadata: NotebookMetadata, format: NotebookFormat, cells: List[Cell]) {
  def toJSON = {
    JSONObject("cells" -> cells.map(_.toJSON), "metadata" -> metadata.toJSON, "language_info" -> languageInfo.toJSON,
    "nbformat" -> format.major, "nbformat_minor" -> format.minor)
  }
}

case class Cell(cellType: String, excecutionCount: Int, metadata: JSONObject, source: List[String], output: List[CellOutput]) {
  def toJSON: JSON = JSONObject(
      "cell_type" -> cellType, "metadata" -> metadata, "source" -> JSONArray(source), "output" -> output.map(_.toJSON)
  )
}
case class CellOutput(data: List[CellOutputPart], metadata: JSONObject, outputType: String) {
  def toJSON = {
    val dataJ = JSONObject(data.map(d => d.mimeType -> JSONArray(d.data)):_*)
    JSONObject("data" -> dataJ, "metadata" -> metadata, "output_type" -> outputType)
  }
}
case class CellOutputPart(mimeType: String, data: List[String])


case class NotebookFormat(major: Int, minor: Int)
case class LanguageInfo(extension: String, mimeType: String, name: String) {
  def toJSON = JSONObject("file_extension" -> extension, "mimetype" -> mimeType, "name" -> name)
}
case class NotebookMetadata(kernelSpec: KernelSpec) {
  def toJSON = JSONObject("kernelspec" -> kernelSpec.toJSON)
}
case class KernelSpec(displayName: String, language: String, name: String) {
  def toJSON = JSONObject("display_name" -> displayName, "language" -> language, "name" -> name)
}

object Notebook {
  val format = NotebookFormat(4,2)
}

object MMTNotebook {
  val MMTKernelSpec = KernelSpec("MMT", "mmt", "mmt")
  val MMTLanguageInfo = LanguageInfo(".mmt", "text/mmt", "mmt")

  def apply(cells: List[Cell]) = Notebook(MMTLanguageInfo, NotebookMetadata(MMTKernelSpec), Notebook.format, cells)
  
  def inTheory(meta: Option[MPath], p: MPath) = {
    val metaS = meta.map(m => ": " + m.toPath).getOrElse("")
    val theoryHeader = MMTCell("theory Scratch : " + metaS + "=")
    val include = MMTCell("include " + p.toPath)
    val end = MMTCell("end")
    MMTNotebook(List(theoryHeader, include, end))
  }
}

object MMTCell {
  def apply(src: String*) = Cell("code", 0, JSONObject(), src.toList, Nil)
}
