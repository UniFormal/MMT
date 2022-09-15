package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.archives.{Archive, BuildTarget}
import info.kwarc.mmt.api.ontology._
import java.io.File

import scala.xml._
import scala.collection.mutable

/** Extension for reading built optimizations from [[GraphOptimizationTool]] and providing annotations
  */
class MMTOptimizationAnnotationReader extends AnnotationProvider{

  val annotationMap: mutable.HashMap[Path, List[Annotation]] = mutable.HashMap[Path, List[Annotation]]()

  /** extracts Path attribute from xml node
    *
    * @param node
    * @return
    */
  private  def nodeToPath(node: Node) : Path = {
    Path.parse(node.attribute("Path").get.head.toString)
  }

  /** simple implementation of [[Annotation]]
    * @param string tooltip string
    * @param marker character to display in the annotation marker
    */
  private case class simpleAnnotation(string : String, marker : Char) extends Annotation{
    override def getMarker: Char = marker
    /** the tooltip for this annotation */
    override def getTooltip: String = string
  }

  /** converts xml node <replaceInclusion...>...</replaceInclusion> to tooltip
    *
    * @param node xml node
    * @return annotation
    */
  private def replaceTooltip(node : Node): Annotation ={
      simpleAnnotation("replace inclusion " + nodeToPath(node) + " with:<br>" +
      (node\"replacement").map({
        r => "&emsp;"+ nodeToPath(r)
      }).mkString("<br>"), 'o')
  }

  /** converts xml node <removeInclusion.../> to tooltip
    *
    * @param node xml node
    * @return annotation
    */
  private def removeTooltip(node : Node): Annotation = {
    simpleAnnotation("remove inclusion " + nodeToPath(node), 'o')
  }

  /** reads optimizations for archive from files
    *
    * searches for .xml files in archiveroot/export/got and adds the suggested optimizations as annotations
    *
    * @param a archive whose optimizations should be read
    * */
  private def readArchive(a : Archive) : Unit = {
    val optFolder = new File(a.root.toString + "/export/got/")
    if (!optFolder.exists || !optFolder.isDirectory) {
      return
    }
    val optFiles = optFolder.listFiles.filter(_.getName.endsWith("xml"))
    optFiles.foreach(
      f => {
        val xml = Utility.trim(XML.loadFile(f.getPath))
        (xml\"theory"\"replaceInclusion").foreach(node => {
          try {annotationMap.put(nodeToPath(node), replaceTooltip(node)::Nil)}
          catch {
            case _ : java.util.NoSuchElementException => Console.err.println("error reading file: " + f.getName)
            case e: Throwable => throw e
          }
        })
        (xml\"theory"\"removeInclusion").foreach(node => {
          try {annotationMap.put(nodeToPath(node), removeTooltip(node)::Nil)}
          catch {
            case _ : java.util.NoSuchElementException => Console.err.println("error reading file: " + f.getName)
            case e : Throwable => throw e
          }
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
    controller.backend.getArchives.foreach(readArchive)
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
