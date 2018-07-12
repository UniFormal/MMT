package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.ontology._
import java.io._
import scala.xml._

import scala.collection.mutable

class MMTOptimizationAnnotationReader extends AnnotationProvider{

  val annotationMap = mutable.HashMap[Path, List[Annotation]]()

  private  def nodeToPath(node: Node) : Path = {
    Path.parse(node.attribute("MPath").get.head.toString())
  }
  /** reads optimizations for archive from files
    *
    * searches for .xml files in archiveroot/export/got and adds the suggested optimizations as annotations
    *
    * @param a archive whose optimizations should be read
    * */
  private def readArchive(a : Archive) : Unit = {
    val optFolder = new File(a.root + "/export/got/")
    if (!optFolder.exists || !optFolder.isDirectory) {
      return
    }
    val optFiles = optFolder.listFiles.filter(_.getName.endsWith("xml"))
    optFiles.map(
      f => {
        val xml = Utility.trim(XML.loadFile(f.getPath))
        (xml\"theory").map(t => {
          annotationMap.put(
            nodeToPath(t),
            (t\"replaceInclusion").toList.map(s => new Annotation {
              override def getMarker: Char = 'o'
              override def getTooltip: String = "replace inclusion " + nodeToPath(s) + " with:<br>" +
                (s\"replacement").map({
                  r => "&emsp;"+ nodeToPath(r)
                }).mkString("<br>")
             }) ++
            (t\"removeInclusion").toList.map(s => new Annotation {
              override def getMarker: Char = 'o'
              override def getTooltip: String = "remove inclusion " + nodeToPath(s)
            }))
        })
      }
    )
  }

  /** reads all suggestions for all archives
    *
    * loads all suggested optimization for all archives from .xml files in their export/got folders
    *
    * @param args unused
    */
  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.backend.getArchives.map(readArchive)
  }

  /** provides optimization annotations for asset
    * this must be fast enough to be called by jEdit every time a line is repainted
    * @param p URI of annotated asset
    * @return list of annotations
    * */
  override def apply(p: Path): List[Annotation] = {
      annotationMap.get(p) match {
        case Some(a) => a
        case _ => Nil
      }
  }
}
