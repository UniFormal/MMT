package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.{ErrorHandler, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.archives.Importer
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.stex.XHTMLParser
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser, HTMLText, SHTMLNode, SHTMLParsingRule}

import scala.util.Try
import scala.xml.NodeSeq


import info.kwarc.mmt.api.archives._
class IliasExporter extends BuildTarget with XHTMLParser {
  val key = "xhtml-ilias"
  val inExts = List("xhtml")
  /*override val inDim = Dim("xhtml")
  override val outExt: String = "xhtml"
  override val outDim: ArchiveDimension = Dim("export", "ilias")
  override def includeFile(name: String): Boolean = name.endsWith(".xhtml")*/
  def wrap(ns : NodeSeq) = {
    val nd = <questestinterop>{ns}</questestinterop>
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE questestinterop SYSTEM \"ims_qtiasiv1p2p1.dtd\">\n" + nd.toString()
  }
  def cleanI(node:HTMLNode)(delete:HTMLNode => Unit) = node.iterate{ n =>
    n.plain._sourceref = None
    n.plain._namespace = HTMLParser.ns_html
    n.plain.attributes.foreach {
      case ((HTMLParser.ns_shtml,"visible"),"false") => delete(n)
      case (p@(HTMLParser.ns_shtml| HTMLParser.ns_mmt | HTMLParser.ns_rustex,_),_) => n.plain.attributes.remove(p)
      case (k@(ns,ik),v) if ns != HTMLParser.ns_html =>
        n.plain.attributes.remove(k)
        n.plain.attributes((HTMLParser.ns_html,ik)) = v
      case _ => // TODO CSS
    }
  }
  def clean(node:HTMLNode) = {
    var todelete : List[HTMLNode] = Nil
    cleanI(node){n => todelete ::= n}
    todelete.foreach(_.delete)
  }
  def item(title:String,uri:String,tp:String)(body : => NodeSeq)(post: =>NodeSeq) =
    <item ident={body.hashCode().toHexString} title={title} maxattempts="0">
    <qticomment>{uri}</qticomment>
    <duration>P0Y0M0DT0H0M0S</duration>
    <itemmetadata>
      <qtimetadata>
        <qtimetadatafield>
          <fieldlabel>QUESTIONTYPE</fieldlabel> <fieldentry>{tp}</fieldentry>
        </qtimetadatafield>
      </qtimetadata>
    </itemmetadata>
      {body}
      {post}
  </item>

  def makeBody(title:String,body: => NodeSeq,after: => NodeSeq) =
    <presentation label={title}><flow><material>
      <mattext texttype="text/html">{body.toString}</mattext>
        </material>{after}</flow>
    </presentation>

  def doMCC(p : Problem) = {
    val mcb = p.tp.get.asInstanceOf[MCB]
    val mccs = mcb.mccs.zipWithIndex
    val after = <response_lid indent="MCR" rcardinality="Multiple">
      <render_choice shuffle="No">{mccs.map { case (n, i) =>
        <response_label ident={i.toString}><material><mattext texttype="text/html">
          {n.toString}
        </mattext></material></response_label>
      }}</render_choice>
    </response_lid>
    val afterafter = NodeSeq.fromSeq({<resprocessing>
      <outcomes><decvar></decvar></outcomes>
      {mccs.flatMap{case (n,i) =>
        NodeSeq.fromSeq(Seq({<respcondition continue="Yes">
          <conditionvar><varequal respident="MCMR">{i.toString}</varequal></conditionvar>
          <setvar action="Add">{if (n.correct) "1" else "-1"}</setvar>
          {if (!n.correct) <displayfeedback feedbacktype="Response" linkrefid={"response_" + i}/>}
        </respcondition>},{<respcondition continue="Yes">
          <conditionvar><not><varequal respident="MCMR">{i.toString}</varequal></not></conditionvar>
          <setvar action="Add">{if (n.correct) "-1" else "1"}</setvar>
          {if (n.correct) <displayfeedback feedbacktype="Response" linkrefid={"response_" + i}/>}
        </respcondition>}))
      }}
    </resprocessing>} :: mccs.map{case (n,i) =>
      <itemfeedback ident={"response_" + i} view="All"><flow_mat><material>
        <mattext texttype="text/html">{n.feedback.map(_.toString).getOrElse("")}</mattext>
      </material></flow_mat></itemfeedback>
    })
    val title = p.path.parent.uri.path.mkString("/") + "?" + p.path.name
    val uri = p.path.toString
    item(title,uri,"MULTIPLE CHOICE QUESTION")(makeBody(title,p.plain.node,after)){afterafter}
  }

  def doFile(inFile:File) = {
    log("Building ilias " + inFile)
    var problems: List[Problem] = Nil
    val state = ProblemState({
      case p: Problem => problems ::= p
      case _ =>
    })
    controller.library.synchronized {
      HTMLParser(inFile)(state)
    }
    problems = problems.filter(_.tp.isDefined)
    problems.foreach(clean)
    problems.map { p =>
      p.tp match {
        case Some(_: MCB) => doMCC(p)
        case Some(_: FillInSol) => // TODO
          ???
        case _ =>
          ???
      }
    }
  }

  override def build(a: Archive, which: Build, in: FilePath, errorCont: Option[ErrorHandler]): Unit = {
    val f = a / Dim("xhtml") / in
    if (f.isFile) {
      buildFile(f,a / Dim("export") / "ilias" / in)
    } else {
      val problems = f.descendants.flatMap(f => try {
        doFile(f)
      } catch {
        case e : Throwable =>
          e.printStackTrace()
          Nil
      })
      val out = (a / Dim("export") / "ilias" / in).setExtension("xml")
      File.write(out, wrap(problems.toSeq))
    }
  }


    def buildFile(inFile:File,outFile:File): BuildResult = {
      val problems = doFile(inFile)
      if (problems.nonEmpty) {
        File.write(outFile.setExtension("xml"),wrap(problems.toSeq))
      }
      BuildSuccess(Nil,Nil)
    }

  case class ProblemState(f : HTMLNode => Unit) extends HTMLParser.ParsingState(controller,rules(f)) {
    var in_solution = false
  }
  private def rules(f:HTMLNode => Unit) = List(
    SHTMLParsingRule("problem", (str, n, _) => {
      val mp = Path.parseM(str)
      val ret = new Problem(mp, n)
      f(ret)
      ret
    }),
    SHTMLParsingRule("multiple-choice-block",(_,n,_) => new MCB(n)),
    SHTMLParsingRule("mcc",(s,n,_) => new MCC(n,s=="true")),
    SHTMLParsingRule("mcc-solution", (_, n, _) => new MCCSol(n)),
    SHTMLParsingRule("fillinsol", (_, n, _) => new FillInSol(n)),
    SHTMLParsingRule("solution", (_, n, _) => new Solution(n)),
  )
  private class Problem(val path:MPath,node:HTMLNode) extends SHTMLNode(node) {
    var tp : Option[IsProblemType] = None
    def copy = {
      val ret = new Problem(path, node.copy)
      ret.tp = tp
      ret
    }
  }
  sealed trait IsProblemType extends SHTMLNode
  private class Solution(node:HTMLNode) extends SHTMLNode(node) {
    def copy=new Solution(node.copy)
    val insol = state.asInstanceOf[ProblemState].in_solution
    state.asInstanceOf[ProblemState].in_solution = true

    override def onAdd: Unit = {
      state.asInstanceOf[ProblemState].in_solution = insol
      this.delete
    }
  }
  private class MCB(node:HTMLNode) extends SHTMLNode(node) with IsProblemType {
    def copy = new MCB(node.copy)
    var mccs : List[MCC] = Nil

    override def onAdd: Unit = {
      super.onAdd
      if (!state.asInstanceOf[ProblemState].in_solution) findAncestor {
        case p: Problem => p.tp = Some(this)
      }
      clean(this)
      this.delete
    }
  }
  private class FillInSol(node:HTMLNode) extends SHTMLNode(node) with IsProblemType {
    def copy = new FillInSol(node.copy)

    var solution: String = ""

    override def onAdd: Unit = {
      this.iterate {
        case txt:HTMLText => solution += txt.text
        case _ =>
          print("")
      }
      super.onAdd
      if (!state.asInstanceOf[ProblemState].in_solution) findAncestor {
        case p: Problem => p.tp = Some(this)
      }
      clean(this)
      this.delete
    }
  }
  private class MCC(node:HTMLNode,val correct:Boolean) extends SHTMLNode(node) {
    def copy = new MCC(node.copy,correct)

    var feedback: Option[MCCSol] = None

    override def onAdd: Unit = {
      super.onAdd
      if (!state.asInstanceOf[ProblemState].in_solution) findAncestor {
        case mcb: MCB => mcb.mccs ::= this
      }
      clean(this)
      this.delete
    }
  }
  private class MCCSol(node:HTMLNode) extends SHTMLNode(node) {
    def copy = new MCCSol(node.copy)

    override def onAdd: Unit = {
      super.onAdd
      if (!state.asInstanceOf[ProblemState].in_solution) findAncestor {
        case mcc: MCC => mcc.feedback = Some(this)
      }
      clean(this)
      this.delete
    }
  }

  /** clean this target in a given archive */
  override def clean(a: Archive, in: FilePath): Unit = {}
}
