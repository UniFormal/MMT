package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{CPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects.{FreeOrAny, OMA, OMID, OMS, Obj, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.{Constant, RuleConstantInterpreter}
import info.kwarc.mmt.api.utils.{HTML, XMLEscaping}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.features.TheoremFeature
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, PreConstant, PreDocument, PreElement, PreFeature, PreParent, PreRuleConstant, PreStructure, PreTheory, ToScript, XHTML, XHTMLArityComponent, XHTMLComplexTerm, XHTMLComponent, XHTMLDecl, XHTMLDefComponent, XHTMLMacroNameComponent, XHTMLModule, XHTMLNode, XHTMLNotationComponent, XHTMLNotationFragment, XHTMLOMBind, XHTMLOMDoc, XHTMLPrecedence, XHTMLStexArg, XHTMLTerm, XHTMLText, XHTMLTypeComponent}

import scala.util.Try
import scala.xml.{Elem, Node}

object OMDocExtension extends DocumentExtension {
  override def start(args: List[String]): Unit = {
    super.start(args)
  }

  class OMDocument(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with ToScript {
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = {
      Some(new PreDocument(resource))
    }
  }
  class XHTMLTheory(initial_node : Option[Node] = None) extends XHTMLModule(initial_node) {
    lazy val language = get()().collectFirst {
      case lang : XHTMLLanguage => lang.resource
    }.getOrElse("")
    lazy val signature = get()().collectFirst {
      case sig : XHTMLSignature => sig.resource
    }.getOrElse("")
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        (signature,language) match {
          case ("","") =>
            Some(new PreTheory(path,p))
          case ("",l) =>
            val sig = new PreTheory(path,p)
            val lt = new PreTheory(path.toDPath ? l,p)
            new PreStructure(path.toString,lt)
            sig.languagemodule = Some(lt)
            lt.signaturemodule = Some(sig)
            Some(sig)
          case (s,l) =>
            val lt = new PreTheory(path.toDPath ? l,p)
            lt.signaturemodule = None
            //lt.add(new PreStructure(path.toString + "/" + s,lt))
            Some(lt)
        }
      case _ =>
        None
    }
  }
  class XHTMLRuleConstant(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with ToScript {
    def path = Path.parseMS(resource,NamespaceMap.empty)
    def args = XHTMLComplexTerm.args(this)

    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p : PreTheory) =>
        p.signaturemodule.foreach(m => new PreRuleConstant(m.path,path,args,rci))
        None
      case _ => None
    }
  }

  class XHTMLVarDecl(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
    private lazy val vdnode : XHTMLNode = {
      val str = children.mkString.trim
      XHTML.applyString(XHTML.unescape(str))(Nil).filter(!_.isInstanceOf[XHTMLText]).head
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


  class XHTMLMetatheoryComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) with ToScript {
    def path = if (resource.isEmpty) {
      get()().collectFirst{
        case i : XHTMLInclude => Try(Path.parseM(i.resource)).toOption
      }.flatten
    } else Try(Path.parseM(resource)).toOption
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p: PreTheory) =>
        p.addMeta(path)
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
    def getMacroname = semanticChildren.collectFirst{case t : XHTMLMacroNameComponent => t.macroname}

    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p : PreTheory) =>
        p.signaturemodule.foreach { m =>
          val pc = new PreConstant(path, m)
          getType.foreach(t => pc.addType(FreeOrAny(t.freeVars, t)))
          getDefiniens.foreach(t => pc.addDefiniens(FreeOrAny(t.freeVars, t)))
          getArity.foreach(pc.arity = _)
          getMacroname.foreach(pc.addMacro)
        }
        None
      case _ => None
    }
  }

  class XHTMLInclude(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p : PreTheory) =>
        if (property == "stex:usemodule") {
          p.languagemodule.map(new PreStructure(resource,_))
        } else p.signaturemodule.map(new PreStructure(resource,_))
      case _ => None
    }
    def path = Path.parseM(resource)
  }

  class XHTMLSignature(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node)
  class XHTMLLanguage(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node)

  import info.kwarc.mmt.stex.xhtml.{XHTMLOMID,XHTMLOMA,XHTMLStexArg,XHTMLOMV,XHTMLOMNum,XHTMLTref}

  override lazy val xhtmlRules = List(
    XHTMLOMDoc.toRule(_ == "stex:namespace")(n => new OMDocument(Some(n))),
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:theory") => new XHTMLTheory(Some(e))},
    XHTMLOMDoc.toRule(_ == "stex:OMA")(n => new XHTMLOMA(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMID")(n => new XHTMLOMID(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMBIND")(n => new XHTMLOMBind(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:arg")(n => new XHTMLStexArg(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:OMV")(n => new XHTMLOMV(Some(n))),
    {case e : Elem if e.label == "mn" => new XHTMLOMNum(Some(e))},
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
    XHTMLOMDoc.toRule(_ == "stex:usemodule")(n => new XHTMLInclude(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:signature")(n => new XHTMLSignature(Some(n))),
    XHTMLOMDoc.toRule(_ == "stex:language")(n => new XHTMLLanguage(Some(n))),
    {case e : Elem if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:tref") =>
      new XHTMLTref(Some(e))}, // TODO
  )

  lazy val rci = new RuleConstantInterpreter(controller)

  import DocumentExtension._

  override lazy val documentRules = List(
    {case thm: XHTMLTheory =>
      sidebar(thm, (<b style="font-size: larger">Theory: {thm.name.toString}</b>) :: Nil)
    },
    {case v: XHTMLVarDecl =>
      val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
      val seq = scala.xml.Text("Variable ") :: server.xhtmlPresenter.asXML(v.vardecl, None) :: is
      sidebar(v, seq)
    },
    {case s: XHTMLSymDecl =>
      controller.getO(s.path) match {
        case Some(c : Constant) =>
          sidebar(s,{<span>Constant {makeButton("/:" + server.pathPrefix + "/declaration?" + c.path,scala.xml.Text(c.name.toString)
          ).node} <code>{PreElement.getMacroName(c).map(s => "(\\" + s + ")").getOrElse("")}</code></span>} :: Nil)
        case _ =>
          ???
      }
    },
    {case s: XHTMLInclude =>
      sidebar(s,{<span>Include {makeButton("/:" + server.pathPrefix + "/theory?" + s.resource,scala.xml.Text(s.path.name.toString)).node}</span>} :: Nil)
    },
    {case t: HasHeadSymbol => overlay(t,"/:" + server.pathPrefix + "/fragment?" + t.head.toString,"/:" + server.pathPrefix + "/declaration?" + t.head.toString)},
    {case v : XHTMLOMV => overlay(v,"/:" + server.pathPrefix + "/fragment?" + v.path,"/:" + server.pathPrefix + "/declaration?" + v.path)},
    {case c : XHTMLTerm if c.attributes.contains(("stex","constant")) => c.toTerm /*{
      sidebar(c, {
        <span>{"Term:"}
          {DocumentExtension.makeButton("/:" + server.pathPrefix + "/declaration?" + c.attributes(("stex", "constant")),
            server.xhtmlPresenter.asXML(c.toTerm, None)).node}
        </span>
      }:: Nil)
    }*/},
    {case c : XHTMLTerm if c.isTop && !c.toTerm.isInstanceOf[OMID] =>
      c.toTerm match {
        case OMS(_) =>
        case _ =>
      }
    }
  )

}