package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._

// note graphviz does not like some characters (including -) in the tooltip attributes, which we use for classes
sealed trait DotObject
/** a node to be used in a [[DotGraph]] */
trait DotNode extends DotObject {
  def id: Path
  def label : String
  /** must be alphanumeric, space-separeated */
  def cls: String
  def toJSON : JSONObject = {
    val ls = ("id",JSONString(id.toString)) :: ("label",JSONString(label)) :: ("style",JSONString(cls)) ::
      ("url",JSONString("/?" + id)) :: Nil
    JSONObject(ls:_*)
  }
}

/** a node to be used in a [[DotGraph]] */
trait DotEdge extends DotObject {
  def from: DotNode
  def to: DotNode
  def id: Option[Path]
  def label: Option[String]
  /** must be alphanumeric, space-separeated */
  def cls: String
  def weight = 1
  @MMT_TODO("")
  def toJSON : JSONObject = {
    val dones = List("graphinclude","graphview","graphstructure","graphmeta","alignment")
    if (!dones.contains(cls)) println(cls)
    val ls = ("from",JSONString(from.id.toString)) ::
      ("to",JSONString(to.id.toString)) ::
      ("weight",JSONInt(weight)) ::
      ("label",JSONString(label.getOrElse(""))) ::
      ("id",JSONString(id.map(_.toString).getOrElse(""))) ::
      ("style",JSONString(cls)) ::
      {if (id.isDefined) ("url",JSONString("/?" + id.get)) :: Nil else Nil}
    JSONObject(ls:_*)
  }
  // TODO Stuff about different arrow types
}

/** a graph that can be converted to SVG by [[DotToSVG]] */
trait DotGraph {
   def title: String
   def nodes: Iterable[DotNode]
   def externalNodes: Option[Iterable[DotNode]]
   def edges: Iterable[DotEdge]

  @MMT_TODO("")
  def JSONNodes : Iterable[JSONObject] = nodes.map(_.toJSON)
  @MMT_TODO("")
  def JSONEdges : Iterable[JSONObject] = edges.map(_.toJSON)
}

/** thrown by [[DotToSVG]] */
case class DotError(m: String) extends Error(m)

/** converts a graph to SVG using the dot tool */
class DotToSVG(dotPath: File) {

  /** dot has a bug, which does not XML escape <>&" in the generated SVG
   * Therefore, this method is used to put escaped values in the dot file to begin with.
   */
   private def esc(s: String) = XMLEscaping(s)

   // note graphviz does not like some characters (including -) in the tooltip attributes
   private def dotNode(n: DotNode) = {
      s""""${n.id}" [label="${esc(n.label)}",tooltip="${esc(n.cls)}",href="${esc(n.id.toPath)}"];"""
   }

   private def dotEdge(e: DotEdge) = {
      val labelAtt = e.label match {
         case None => ""
         case Some(l) => s"""label="${esc(l)}","""
      }
      val refAtt = e.id.map(p => s"""href="${esc(p.toPath)}",""").getOrElse("")
      s""""${e.from.id.toPath}" -> "${e.to.id.toPath}" [${labelAtt}${refAtt}tooltip="${esc(e.cls)}",weight=${e.weight}];"""
   }

   private def toDot(dg: DotGraph, f: File): Unit = {
       val file = File.Writer(f)
       def write(s: String): Unit = {file.println(s)}

       // all nodes that have been added
       var nodesDone: List[DotNode] = Nil
       // add the minimal theories

       def addNode(n: DotNode): Unit = {
          if (! nodesDone.contains(n)) {
            write(dotNode(n))
            nodesDone ::= n
          }
       }

       write(s"digraph ${dg.title} {")

       // add the nodes, wrapped in a cluster if there any external nodes
       if (dg.externalNodes.isDefined)
          write("subgraph cluster_local {")
       dg.nodes foreach addNode
       if (dg.externalNodes.isDefined)
          write("}")
       dg.externalNodes map {ens => ens foreach addNode}

       // add the edges
       dg.edges foreach {edge =>
          addNode(edge.from)
          addNode(edge.to)
          write(dotEdge(edge))
       }

       write("}\n")
       file.close
    }

    private def adaptSVG(svg: String) = {
       // we use the title attribute as a css class and remove the default style so that we can style with css
       val svgR = svg.replace("stroke=\"black\"", "").replace("<svg ", "<svg style=\"stroke:black\" ")
         .replace("xlink:title", "class")
         .replace("fill=\"black\"", "")
         .replace("xlink:href", presentation.HTMLAttributes.symref)
         .replaceAll("<!-- Generated by graphviz version.*\\s* -->", "")
       val i = svgR.indexOf("<svg")
       svgR.substring(i)
    }

    /**
     * @param dg the graph to layout
     * @return the svg graph returned by dot
     */
    def apply(dg: DotGraph): String = {
      val outFile = File(System.getProperty("java.io.tmpdir")) / "graphviz.svg"
      val dotFile = outFile.setExtension("dot")
      toDot(dg, dotFile)
      val result = ShellCommand.run(dotPath.toString, "-Tsvg", "-o" + outFile, dotFile.toString)
      result match {
        case ShellCommand.Abort(e) => throw DotError("error while running dot").setCausedBy(e)
        case ShellCommand.Fail(m, _) => throw DotError(m)
        case _: ShellCommand.Success =>
      }
      //dotFile.delete
      val svg = File.read(outFile)
      adaptSVG(svg)
    }
}
