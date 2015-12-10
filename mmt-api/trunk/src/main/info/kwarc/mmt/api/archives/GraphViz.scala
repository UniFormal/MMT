package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._

/**
 * uses [[ontology.GraphExporter]] to produce a dot file and then calls dot to produce an svg file
 */
class GraphViz extends Exporter {
  val key = "svg"

  private var tg: ontology.TheoryGraph = null
  /** path to graphviz (dot) binary */
  private var graphviz: String = "dot"

  /** expects one argument: the path to graphviz; alternatively set variable GraphViz */
  override def start(args: List[String]): Unit = {
    super.start(args)
    tg = new ontology.TheoryGraph(controller.depstore)
    graphviz = getFromFirstArgOrEnvvar(remainingStartArguments, "GraphViz", graphviz)
  }

  /** contains at least all elements of the document */
  def exportDocument(doc: Document, bf: BuildTask): Unit = {
    val theories = controller.depstore.querySet(doc.path, Transitive(+Declares) * HasType(IsTheory))
    val views = controller.depstore.querySet(doc.path, Transitive(+Declares) * HasType(IsView))
    produceGraph(theories, views, bf)
  }

  /** contains at least the theory */
  def exportTheory(thy: DeclaredTheory, bf: BuildTask): Unit = {
    produceGraph(List(thy.path), Nil, bf)
  }

  /** contains at least domain, codomain, and view */
  def exportView(view: DeclaredView, bf: BuildTask): Unit = {
    val theories = List(view.from, view.to).flatMap(objects.TheoryExp.getSupport)
    produceGraph(theories, List(view.path), bf)
  }

  /** nothing for now */
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit = {}

  private def produceGraph(theories: Iterable[Path], views: Iterable[Path], bt: BuildTask): Unit = {
    val outStringPostProcessed = getSVG(theories, views, Some(bt.outFile), Some(tg), m => bt.errorCont(LocalError(m)))
    File.write(bt.outFile, outStringPostProcessed)
  }

  def getSVG(theories: Iterable[Path], views: Iterable[Path], file: Option[File], graph: Option[TheoryGraph],
             errorcont: String => Unit): String = {
    val outFile = file.getOrElse(File(System.getProperty("java.io.tmpdir")) / "MMTGraph.svg")
    val gv = new ontology.GraphExporter(theories, views, graph match {
      case Some(g) => g
      case None => tg
    })
    val dotFile = outFile.setExtension("dot")
    gv.exportDot(dotFile)
    val result = ShellCommand.run(graphviz, "-Tsvg", "-o" + outFile, dotFile.toString)
    result foreach { m => errorcont(m) }
    dotFile.delete
    //TODO remove width/height attributes of svg element to allow for automatic resizing in the browser
    File.read(outFile).replace("xlink:title", "class").replace("xlink:href", presentation.HTMLAttributes.symref)
  }
}
