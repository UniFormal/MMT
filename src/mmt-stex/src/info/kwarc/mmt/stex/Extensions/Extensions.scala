package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{CPath, DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects.{Free, FreeOrAny, OMA, OMAorAny, OMID, OMMOD, OMS, Obj, Term, VarDecl}
import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.symbols.{RuleConstant, RuleConstantInterpreter}
import info.kwarc.mmt.api.utils.{MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.features.TheoremFeature
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, PreConstant, PreDocument, PreElement, PreFeature, PreParent, PreRuleConstant, PreStructure, PreTheory, ToScript, XHTML, XHTMLArityComponent, XHTMLComponent, XHTMLDecl, XHTMLDefComponent, XHTMLMacroNameComponent, XHTMLModule, XHTMLNode, XHTMLNotationComponent, XHTMLNotationFragment, XHTMLOMDoc, XHTMLPrecedence, XHTMLSidebar, XHTMLStexArg, XHTMLTerm, XHTMLText, XHTMLTypeComponent}
import info.kwarc.mmt.stex.{STeX, STeXServer}

import scala.util.Try
import scala.xml.{Elem, Node}

abstract class Translator(val language : String) {
  def applicable(tm : Term) : Boolean
  def translate(tm : Term) : (Term,List[GlobalName])
}

trait STeXExtension extends Extension {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  def xhtmlRules : List[PartialFunction[Node,XHTMLNode]] = Nil
  def serverReturn(request: ServerRequest): Option[ServerResponse] = None
  def documentRules : List[PartialFunction[(XHTMLNode,XHTMLNode => Unit),Unit]] = Nil
  def translators : List[Translator] = Nil
}

object BasicExtension extends STeXExtension {
  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(new TheoremFeature)
  }

  class OMDocument(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = {
      Some(new PreDocument(resource))
    }
  }
  class XHTMLTheory(initial_node : Option[Node] = None) extends XHTMLModule(initial_node) {
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) => Some(new PreTheory(path,p))
      case _ => None
    }
  }
  class XHTMLTheorem(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
    def path = Path.parseS(resource,NamespaceMap.empty)
    def module = path.module
    def name = path.name
    def dpath = module.parent
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        val f = new PreFeature(path, "stex:theorem", p)
        val thpath = path.module / (path.name.toString + "_feature")
        semanticChildren.foreach{
          case t : XHTMLTerm =>
            val c = new PreConstant(thpath ? f.newName("conclusion"),f)
            c.addRole("conclusion")
            val term = t.toTerm
            c.addDefiniens(FreeOrAny(term.freeVars,term))
          case _ =>
        }
        Some(f)
      case _ => None
    }
  }

  class XHTMLRuleConstant(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with ToScript {
    def path = Path.parseMS(resource,NamespaceMap.empty)
    def args = semanticChildren.collect({case a : XHTMLStexArg => a}).map{arg =>
      arg.semanticChildren.collect {
        case t : XHTMLTerm => t
      } match {
        case List(a) =>
          (arg.number,a.toTerm)
        case _ =>
          ???
      }
    }.sortBy(_._1).map(_._2)

    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p : PreTheory) =>
        Some(new PreRuleConstant(p.path,path,args,rci))
      case _ => None
    }
  }

  class XHTMLVarDecl(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
    def unescape(s : String) : String = {
      val s1 = XMLEscaping.unapply(s)
      if (s1 == s) s else unescape(s1)
    }
    private lazy val vdnode : XHTMLNode = {
      val str = children.mkString.trim
      XHTML.applyString(unescape(str))(Nil).filter(!_.isInstanceOf[XHTMLText]).head
    }
    override def path = Path.parseS(vdnode.attributes.getOrElse(("","path"),{
      print("")
      ???
    }))
    override def name = path.name
    def notation = vdnode.attributes.getOrElse(("","notation"),name.toString)
    def universal = vdnode.attributes.get(("","quantified")) match {
      case Some("universal") => Some(true)
      case Some("existential") => Some(false)
      case _ => None
    }
    def tp : Option[Term] = vdnode.children.find(_.label == "type").map(_.children.head.node).map(Obj.parseTerm(_,NamespaceMap.empty))
    def vardecl = VarDecl(name,None,tp,None,if (notation.isEmpty) None else Some(TextNotation.parse(notation,NamespaceMap.empty)))

    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        val pc = new PreConstant(path,p)
        pc.metadata(STeX.meta_quantification) = OMS(
          if (universal.contains(true) || universal.isEmpty) STeX.Forall.path
          else STeX.Exists.path
        )
        pc.addRole("Variable")
        tp.foreach(t => pc.addType(FreeOrAny(t.freeVars,t)))
        Some(pc)
      case _ => None
    }
  }


  class XHTMLMetatheoryComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
    def path = if (resource.isEmpty) {
      get()().collectFirst{
        case i : XHTMLInclude => Try(Path.parseM(i.resource)).toOption
      }.flatten
    } else Try(Path.parseM(resource)).toOption
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p: PreTheory) =>
        p.metatheory = path
        None
      case _ => None
    }
  }

  class XHTMLNotation(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
    def fragment = semanticChildren.collectFirst{case f : XHTMLNotationFragment => f.fragment}.getOrElse{
      print("")
      ???
    }
    def notation = semanticChildren.collectFirst{case f : XHTMLNotationComponent => f.notation(path)}.getOrElse{
      print("")
      ???
    }
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        val symbol = path
        p.find{case pc : PreConstant if pc.path == symbol => true case _ => false} match {
          case Some(pc : PreConstant) =>
            pc.addNotation(fragment,notation)
          case _ =>
            print("")
            ???
          // TODO
        }
        None
      case _ => None
    }
  }

  class XHTMLSymDecl(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
    def getType = semanticChildren.collectFirst{case t : XHTMLTypeComponent => t.getTerm}.flatten
    def getDefiniens = semanticChildren.collectFirst{case t : XHTMLDefComponent => t.getTerm}.flatten
    def getArity = semanticChildren.collectFirst{case t : XHTMLArityComponent => t.arity}

    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        val pc = new PreConstant(path,p)
        getType.foreach(t => pc.addType(FreeOrAny(t.freeVars,t)))
        getDefiniens.foreach(t => pc.addDefiniens(FreeOrAny(t.freeVars,t)))
        getArity.foreach(pc.arity = _)
        Some(pc)
      case _ => None
    }
  }

  class XHTMLInclude(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        Some(new PreStructure(resource,p))
      case _ => None
    }
  }

  import info.kwarc.mmt.stex.xhtml.{XHTMLOMID,XHTMLOMA,XHTMLStexArg,XHTMLOMV,XHTMLOMNum,XHTMLTref}

  override lazy val xhtmlRules = List(
    { case n if n.label == "span" && n.attributes.asAttrMap.get("property").contains("stex:namespace") => new OMDocument(Some(n))},
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:theory") => new XHTMLTheory(Some(e))},
    XHTMLOMDoc.toRule(_ == "stex:OMA")(n => new XHTMLOMA(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMID")(n => new XHTMLOMID(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:arg")(n => new XHTMLStexArg(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMV")(n => new XHTMLOMV(Some(n))),
    {case e : Elem if e.label == "mn" => new XHTMLOMNum(Some(e))},
    {case e : Elem if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:tref") =>
      new XHTMLTref(Some(e))}, // TODO
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:feature:theorem") =>
      new XHTMLTheorem(Some(e))}, // TODO
    {case e : Elem if e.label == "script" && e.attributes.asAttrMap.get("property").contains("stex:vardecl") =>
      new XHTMLVarDecl(Some(e))},
    XHTMLOMDoc.toRule(_ == "stex:symdecl")(n => new XHTMLSymDecl(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:type")(n => new XHTMLTypeComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:definiens")(n => new XHTMLDefComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:args")(n => new XHTMLArityComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:notationfragment")(n => new XHTMLNotationFragment(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:precedence")(n => new XHTMLPrecedence(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:notationcomp")(n => new XHTMLNotationComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:notation")(n => new XHTMLNotation(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:macroname")(n => new XHTMLMacroNameComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:mmtrule")(n => new XHTMLRuleConstant(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:metatheory")(n => new XHTMLMetatheoryComponent(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:import")(n => new XHTMLInclude(Some(n))),

  )

  lazy val rci = new RuleConstantInterpreter(controller)

  def termLink(o : Obj, comp : Option[CPath]) = {
    <form method="post" action={"/:" + server.pathPrefix + "/expression"} class="inline btncont" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner">
      <input type="hidden" name="openmath" value={o.toNode.toString().replace("\n","").replace("\n","")}/>
      <a onclick="this.closest('form').submit();" type="submit" name="component" value={comp.map(_.toString).getOrElse("None")} class="propbtn">
        {server.xhtmlPresenter.asXML(o, comp)}
      </a>
    </form>
    //   <a href ={"/:" + this.pathPrefix + "/expression/???"}  target="_blank">{presenter.asXML(o, comp)}</a>
  }

  override lazy val documentRules = List(
    {case (thm: XHTMLTheory,rec) =>
      val sidebar = XHTMLSidebar(thm.path.toString, scala.xml.Text("Theory: " + thm.name.toString))
      thm.children.headOption match {
        case Some(h) =>
          thm.addBefore(sidebar,h)
        case _ =>
          thm.add(sidebar)
      }
      rec(sidebar)
    },
    { case (thm: XHTMLTheorem,rec) =>
      val c = controller.getConstant(thm.path)
      val sb = <div>
        {scala.xml.Text("Theorem " + thm.name.toString + ":\n")}{termLink(c.tp match {
          case Some(OMA(OMS(STeX.ded),List(a))) => a
          case _ =>
            ???
        }, Some(c.path $ TypeComponent))}
      </div>
      val sidebar = XHTMLSidebar(thm.path.toString, sb: _*)
      thm.add(sidebar)
      rec(sidebar)
    },
    {case (v: XHTMLVarDecl,rec) =>
      val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
      val seq = scala.xml.Text("Variable ") :: server.xhtmlPresenter.asXML(v.vardecl, None) :: is
      val sidebar = XHTMLSidebar(v.name.toString, seq: _*)
      v.parent.foreach(_.addBefore(sidebar, v))
      rec(sidebar)
    },
    {case (t: HasHeadSymbol,_) => t.addOverlay("/:" + server.pathPrefix + "/fragment?" + t.head.toString)},
    {case (v : XHTMLOMV,_) => v.addOverlay("/:" + server.pathPrefix + "/fragment?" + v.path)}
  )
}