package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{CPath, DefComponent, GlobalName, MPath, Path, StructuralElement}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.{Obj, Term}
import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.utils.MMTSystem
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.odk.Sage.{Sage, SageSystem}
import info.kwarc.mmt.stex.{STeXServer, translations}
import info.kwarc.mmt.stex.translations.DemoContent
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, XHTML, XHTMLNode, XHTMLOMDoc, XHTMLSidebar}

import scala.xml.{Elem, Node}

abstract class Translator(val language : String) {
  def applicable(tm : Term) : Boolean
  def translate(tm : Term) : (Term,List[GlobalName])
}

trait STeXExtension extends Extension {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  def xhtmlRules : List[PartialFunction[Node,XHTMLNode]] = Nil
  def serverReturn(request: ServerRequest): Option[ServerResponse] = None
  def extractionRules : List[PartialFunction[XHTMLNode,StructuralElement]] = Nil
  def documentRules : List[PartialFunction[(XHTMLNode,List[PartialFunction[Node,XHTMLNode]],XHTMLNode => Unit),Unit]] = Nil
  def translators : List[Translator] = Nil
}

object BasicExtension extends STeXExtension {
  import info.kwarc.mmt.stex.xhtml.{XHTMLOMA, XHTMLOMID, XHTMLOMNum, XHTMLOMV, XHTMLStexArg, XHTMLTheorem, XHTMLTheory, XHTMLTref, XHTMLVarDecl}
  override lazy val xhtmlRules = List(
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:theory") =>
      new XHTMLTheory(Some(e))},
    XHTMLOMDoc.toRule(_ == "stex:OMA")(n => new XHTMLOMA(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMID")(n => new XHTMLOMID(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:arg")(n => new XHTMLStexArg(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMV")(n => new XHTMLOMV(Some(n))),
    {case e : Elem if e.label == "mn" => new XHTMLOMNum(Some(e))},
    {case e : Elem if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:tref") =>
      new XHTMLTref(Some(e))},
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:theorem") =>
      new XHTMLTheorem(Some(e))},
    {case e : Elem if e.label == "script" && e.attributes.asAttrMap.get("property").contains("stex:vardecl") =>
      new XHTMLVarDecl(Some(e))},
  )

  override lazy val extractionRules = List(
    {case thm: XHTMLTheory => thm.toModule(controller)},
    {case thm: XHTMLTheorem => thm.toDeclaration},
    {case v: XHTMLVarDecl => v.toDeclaration}
  )

  def termLink(o : Obj, comp : Option[CPath]) = {
    <form method="post" action={"/:" + server.pathPrefix + "/expression"} class="inline" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner">
      <input type="hidden" name="openmath" value={o.toNode.toString().replace("\n","").replace("\n","")}/>
      <button type="submit" name="component" value={comp.map(_.toString).getOrElse("None")}>
        {server.presenter.asXML(o, comp)}
      </button>
    </form>
    //   <a href ={"/:" + this.pathPrefix + "/expression/???"}  target="_blank">{presenter.asXML(o, comp)}</a>
  }

  override lazy val documentRules = List(
    {case (thm: XHTMLTheory,rules,rec) =>
      val sidebar = XHTMLSidebar(thm.path.toString, scala.xml.Text("Theory: " + thm.name.toString))
      thm.children.headOption match {
        case Some(h) =>
          thm.addBefore(sidebar,h)
        case _ =>
          thm.add(sidebar)
      }
      rec(sidebar)
    },
    { case (thm: XHTMLTheorem,rules,rec) =>
      val c = thm.toDeclaration
      val sb = <div>
        {scala.xml.Text("Theorem " + thm.name.toString + ":\n")}{termLink(c.df.get, Some(c.path $ DefComponent))}
      </div>
      val sidebar = XHTMLSidebar(thm.path.toString, sb: _*)
      thm.add(sidebar)
      rec(sidebar)
    },
    {case (v: XHTMLVarDecl,rules,rec) =>
      val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
      val seq = scala.xml.Text("Variable ") :: server.presenter.asXML(v.vardecl, None) :: is
      val sidebar = XHTMLSidebar(v.name.toString, seq: _*)
      v.parent.foreach(_.addBefore(sidebar, v))
      rec(sidebar)
    },
    {case (t: HasHeadSymbol,_,_) => t.addOverlay("/:" + server.pathPrefix + "/fragment?" + t.head.toString)},
    {case (v : XHTMLOMV,_,_) => v.addOverlay("/:" + server.pathPrefix + "/fragment?" + v.path)}
  )
}

object DemoExtension extends STeXExtension {
  override def start(args: List[String]): Unit = {
    super.start(args)
    DemoContent.add(controller)
    implicit val xhtmlrules = XHTML.Rules.defaultrules ::: BasicExtension.xhtmlRules
    val filecontent = XHTML.applyString(MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml")).head
    filecontent.iterate(node => BasicExtension.extractionRules.foreach(_.unapply(node).foreach(controller.add(_))))
  }
  override def serverReturn(request: ServerRequest): Option[ServerResponse] = {
    if (request.path.tail.isEmpty)
      Some(ServerResponse(MMTSystem.getResourceAsString("/mmt-web/stex/demo/index.html"),"html"))
    else None
  }

  override lazy val translators: List[Translator] = {
    val mitm_translator = new Translator("MitM") {
      override def applicable(tm: Term): Boolean = true
      val trl = translations.MitM.getTranslator(controller)
      override def translate(tm: Term): (Term, List[GlobalName]) = trl.translate(tm)
    }
    val sagesys = controller.extman.get(classOf[SageSystem]) match {
      case ss :: _ => ss
      case _ =>
        val ss = new SageSystem
        controller.extman.addExtension(ss)
        ss.warmup()
        ss
    }
    val sage_translator = new Translator("Sage") {
      override def applicable(tm: Term): Boolean = true
      val trl = sagesys.translator_to
      override def translate(tm: Term): (Term, List[GlobalName]) = trl.translate(tm)
    }
    List(mitm_translator,sage_translator)
  }
}