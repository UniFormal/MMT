package info.kwarc.mmt.stex.demo

import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{CPath, ContentPath, DPath, DefComponent, GlobalName, LocalName, NamespaceMap, Path, TypeComponent}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{Delim, Marker, NotationContainer, SimpArg, TextNotation, Var}
import info.kwarc.mmt.api.objects.{Context, OMA, OMBIND, OMLITTrait, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, AlignmentTranslation, LinkTranslation, TranslationGroup, TranslationTarget}
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.{File, MMTSystem, URI, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lf.{ApplySpine, Arrow, Lambda, Typed}
import info.kwarc.mmt.odk.{LFX, NatLiterals}
import info.kwarc.mmt.stex.{InformalMathMLPresenter, MMTInformalPresenter, STeX}
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, XHTML, XHTMLDecl, XHTMLDocument, XHTMLElem, XHTMLNode, XHTMLOMV, XHTMLSidebar, XHTMLTerm, XHTMLTheorem, XHTMLTheory, XHTMLVarDecl}

import scala.xml.{Node, NodeSeq}

class DemoServer extends ServerExtension("stexdemo") {
  private var initialized = false

  override def start(args: List[String]): Unit = {
    super.start(args)
    initialize
  }

  def initialize = if (!initialized) {
    import Content._
    controller.add(th_set)
    controller.add(c_set)
    controller.add(th_nat)
    controller.add(PlainInclude(th_set.path,th_nat.path))
    controller.add(c_nat)
    controller.add(th_prop)
    controller.add(PlainInclude(th_set.path,th_prop.path))
    controller.add(c_prop)
    controller.add(th_fun)
    controller.add(PlainInclude(th_set.path,th_fun.path))
    controller.add(c_fun)
    controller.add(th_impl)
    controller.add(PlainInclude(th_fun.path,th_impl.path))
    controller.add(PlainInclude(th_prop.path,th_impl.path))
    controller.add(c_impl)
    controller.add(th_div)
    controller.add(PlainInclude(th_fun.path,th_div.path))
    controller.add(PlainInclude(th_prop.path,th_div.path))
    controller.add(PlainInclude(th_nat.path,th_div.path))
    controller.add(c_even)
    controller.add(th_exp)
    controller.add(PlainInclude(th_fun.path,th_exp.path))
    controller.add(PlainInclude(th_nat.path,th_exp.path))
    controller.add(c_natexp)
    controller.add(th_forall)
    controller.add(PlainInclude(th_prop.path,th_forall.path))
    controller.add(c_forall)
    controller.add(th_exists)
    controller.add(PlainInclude(th_prop.path,th_exists.path))
    controller.add(c_exists)
  }

  lazy val mitm_translator = new AcrossLibraryTranslator(controller,Content.translations,Nil,((path: GlobalName, _: Controller) =>
    Typed._base <= path ||
    LFX.ns <= path ||
    MitM.basepath <= path),false)

  lazy val presenter = controller.extman.get(classOf[MMTInformalPresenter]) match {
    case p :: _ => p
    case Nil =>
      val p = new MMTInformalPresenter
      controller.extman.addExtension(p)
      p
  }

  def doHeader(doc : XHTMLNode) = {
    val head = doc.get("head")().head
    head.get("link")(("","rel","stylesheet")).foreach(e => e.attributes.get(("","href")) match {
      case Some("https://latex.now.sh/style.css") => e.delete
      case Some("LaTeXML.css") => e.attributes(("","href")) = "/stex/latexml/LaTeXML.css"
      case Some(s) if s.startsWith("ltx-") => e.attributes(("","href")) = "/stex/latexml/" + s
      case _ =>
    })
    head.add(<link rel="stylesheet" href="/stex/latex-css/style.css"/>)
    head
  }

  def doMainHeader(doc : XHTMLNode): Unit = {
    val p = XHTML.empty
    val head = doHeader(doc)
/*
    (  <script type="text/javascript" src="script/jquery/jquery.js">{p}</script>
        <link rel="stylesheet" type="text/css" href="css/bootstrap-jobad/css/bootstrap.less.css"/>
        <link rel="stylesheet" type="text/css" href="css/mmt.css" />
        <link rel="stylesheet" type="text/css" href="css/browser.css" />
        <link rel="stylesheet" type="text/css" href="css/JOBAD.css" />
        <link rel="stylesheet" type="text/css" href="css/jquery/jquery-ui.css"/>
      <script type="text/javascript" src="script/jquery/jquery-ui.js">{p}</script>
      <script type="text/javascript" src="script/tree/jquery.hotkeys.js">{p}</script>
      <script type="text/javascript" src="script/tree/jquery.jstree.js">{p}</script>
      <script type="text/javascript" src="script/incsearch/treeview.js">{p}</script>
        <link rel='stylesheet' href='css/incsearch/jstree.css'/>
        <link rel='stylesheet' href='css/incsearch/index.css'/>
        <link rel='stylesheet' href='css/incsearch/incsearch.css'/>
        <link rel='stylesheet' href='css/incsearch/treeview.css'/>
      <script type="text/javascript" src="script/mmt/mmt-js-api.js">{p}</script>
      <script type="text/javascript" src="script/jobad/deps/underscore-min.js">{p}</script>
      <script type="text/javascript" src="script/bootstrap2/bootstrap.js">{p}</script>
      <script type="text/javascript" src="script/jobad/JOBAD.js">{p}</script>
      <script type="text/javascript" src="script/jobad/modules/hovering.js">{p}</script>
      <script type="text/javascript" src="script/jobad/modules/interactive-viewing.js">{p}</script>
      <script type="text/javascript" src="script/mmt/browser.js">{p}</script>).toList.foreach(head.add(_))
 */
  }

  def emptydoc = {
    val dummynode = <div></div>
    val doc = new XHTMLDocument(dummynode)(XHTML.Rules.defaultrules)
    doc.add(<head><meta http-equiv="Content-Type" content="application/xhtml+xml; charset=UTF-8"/></head>)
    doc.add(<body><div class="ltx_page_main"><div class="ltx_page_content"><div class="ltx_document"></div></div></div></body>)
    doc
  }

  def doDeclaration(s : String) = {
    val path = Path.parseS(s)
    val c = controller.getConstant(path)
    val header = """<!DOCTYPE html>
                   |<html xmlns="http://www.w3.org/1999/xhtml"
                   |  xmlns:om="http://www.openmath.org/OpenMath"
                   |  xmlns:stex="http://www.mathhub.info"
                   |  xmlns:ml="http://www.w3.org/1998/Math/MathML">
                   |<head></head>
                   |<body>""".stripMargin
    val (prefix,suffix) = c.rl match {
      case Some("variable") =>
        ("Variable",c.metadata.getValues(STeX.meta_quantification) match {
            case List(OMS(STeX.Forall.path)) => " (universally quantified)"
            case List(OMS(STeX.Exists.path)) => " (existentially quantified)"
            case _ => ""
          })
      case _ =>
        ("Symbol","")
    }
    val content = prefix + " <a href=\"/?"+s+"\" target=\"_blank\">" + XMLEscaping(c.path.toString) +
      "</a>:<br><table><tr><th>Macro</th><th>Presentation</th><th>Type</th><th></th></tr><tr><td>" + {
      c.notC.parsing match {
        case Some(tn) => tn.markers.mkString("")
        case _ => "(None)"
      }
    } + "</td><td>" + {
      c.notC.presentation match {
        case Some(tn) => tn.markers.mkString("")
        case _ => "(None)"
      }
    } + "</td><td>" +
    {
      c.tp match {
        case Some(tpi) => presenter.asXML(tpi,Some(c.path $ TypeComponent)).toString()
        case _ => "(None)"
      }
    } + "</td><td>" + suffix + "</td></tr></table>"
    val footer = "</body>\n</html>"
    XHTML.applyString(header + content + footer)(XHTML.Rules.defaultrules).head
  }

  def getFragment(s:String) : String = MMTSystem.getResourceAsString("mmt-web" + (s match {
    case _ if s == Content.c_nat.path.toString =>
      "/stex/demo/naturalnumbers.en.xhtml"
    case _ if s == Content.c_impl.path.toString =>
      "/stex/demo/implication.en.xhtml"
    case _ if s == Content.c_even.path.toString =>
      "/stex/demo/even.en.xhtml"
    case _ if s == Content.c_natexp.path.toString =>
      "/stex/demo/exponentiation.en.xhtml"
    case _ => return "missing"
  }))

  def doFragment(uri : String) = {
    import info.kwarc.mmt.stex.xhtml.XHTML.Rules._
    val frag = getFragment(uri) match {
      case "missing" => None
      case o => Some(o)
    }
    val decl = doDeclaration(uri).get("body")().head.children
    val (filecontent,default) = frag.map { f =>
      (XHTML.applyString(f).head,false)
    }.getOrElse((emptydoc,true))
    doHeader(filecontent)
    val body = filecontent.get("body")().head
    body.attributes(("", "style")) = "margin:0;padding:0;"
    val doc = body.get("div")(("", "class", "ltx_page_main")).head
    doc.attributes(("", "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
    doc.get("div")().foreach { e =>
      if (e.attributes.get(("", "class")).exists(_.contains("ltx_theorem"))) {
        e.attributes(("", "style")) = "margin:0;"
      }
    }
    val border = new XHTMLElem(<div style="font-size:small">{decl.map(_.node)}{ if (!default) <hr/>}</div>,None)
    doc.children.foreach {c =>
      c.delete
      border.add(c,None)
    }
    doc.add(border,None)
    filecontent
  }

  def getDocument(uri : String) : String = uri match {
    case "http://mathhub.info/fomtex/demo.xhtml" =>
      MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml")
    case _ => ???
  }

  def termLink(o : Obj, comp : Option[CPath]) = { // TODO should be POST
    <form method="get" action={"/:" + this.pathPrefix + "/expression"} class="inline">
      <input type="hidden" name="openmath" value={o.toNode.toString().replace("\n","").replace("\n","")}/>
      <button type="submit" name="component" value={comp.map(_.toString).getOrElse("None")} class="link" target="_blank">
        {presenter.asXML(o, comp)}
      </button>
    </form>
 //   <a href ={"/:" + this.pathPrefix + "/expression/???"}  target="_blank">{presenter.asXML(o, comp)}</a>
  }

  private def toLatex(notation : List[Marker],args : List[Obj]) : String = notation.map{
    case Delim(s) => s
    case Var(i,_,_,_) =>
      val v = args(i-1) match {
        case Context(vd) => vd
        case vd : VarDecl => vd
        case _ =>
          ???
      }
      toLaTeX(v)
    case SimpArg(i,_) => toLaTeX(args(i-1))
    case _ =>
      ???
  }.mkString("")
  def toLaTeX(o : Obj) : String = o match {
    case OMBIND(OMS(f),ctx,bd) =>
      controller.getConstant(f).notC.parsing match {
        case Some(tn) =>
          toLatex(tn.markers,ctx :: bd :: Nil)
        case _ =>
          ???
      }
    case OMA(OMS(f),args) =>
      controller.getConstant(f).notC.parsing match {
        case Some(tn) =>
          toLatex(tn.markers,args)
        case _ =>
          ???
      }
    case OMV(n) => "\\" + n.toString
    case OMS(s) =>
      controller.getConstant(s).notC.parsing match {
        case Some(tn) =>
          toLatex(tn.markers,Nil)
        case _ =>
          ???
      }
    case vd:VarDecl => "\\" + vd.name.toString
    case o : OMLITTrait => o.valueString
    case _ =>
      print("")
      ???
  }

  def doExpression(o : Obj,src:Option[CPath]) = {
    val doc = emptydoc
    doMainHeader(doc)
    val body = doc.get("div")(("","class","ltx_document")).head
    body.add(scala.xml.Text("Expression: "))
    body.add(presenter.asXML(o,src))
    body.iterate {
      case e if e.label == "mo" =>
        e.attributes.get(("","data-mmt-symref")) match {
          case Some(str) =>
            e.addOverlay("/:" + this.pathPrefix + "/fragment?" + str)
          case _ =>
        }
      case _ =>
    }
    body.add(<br/>)
    body.add(scala.xml.Text("LaTeX: "))
    body.add(<code><pre>{scala.xml.Text(toLaTeX(o))}</pre></code>)
    doc
  }

  def doDocument(uri : String) = {
    import info.kwarc.mmt.stex.xhtml.XHTMLTerm._

    val filecontent = XHTML.applyString(getDocument(uri)).head
    doMainHeader(filecontent)

    filecontent.iterate {
      case thm: XHTMLTheory =>
        thm.toModule(controller)
        thm.add(XHTMLSidebar(thm.path.toString, scala.xml.Text("Theory: " + thm.name.toString)), thm.children.headOption)
      case thm: XHTMLTheorem =>
        val c = thm.toDeclaration
        controller add c
        val sb = <div>{scala.xml.Text("Theorem " + thm.name.toString + ":\n")}{termLink(c.df.get, Some(c.path $ DefComponent))}</div>
        thm.add(XHTMLSidebar(thm.path.toString, sb: _*), None)
      case v: XHTMLVarDecl =>
        val decl = v.toDeclaration
        controller add decl
        val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
        val seq = scala.xml.Text("Variable ") :: presenter.asXML(v.vardecl, None) :: is
        v.parent.foreach(_.add(XHTMLSidebar(v.name.toString, seq: _*), Some(v)))
      case _ =>
    }
    filecontent.iterate {
      case t: HasHeadSymbol =>
        t.addOverlay("/:" + this.pathPrefix + "/fragment?" + t.head.toString)
      case v : XHTMLOMV =>
        v.addOverlay("/:" + this.pathPrefix + "/fragment?" + v.path)
      case _ =>
    }
    filecontent
  }

  override def apply(request: ServerRequest): ServerResponse = try {
    initialize
    val ret = request.path.lastOption match {
      case Some("fragment") =>
        request.query match {
          case "" =>
            ???
          case s => doFragment(s)
        }
      case Some("document") =>
        request.query match {
          case "" =>
            ???
          case s =>
            doDocument(s)
        }
      case Some("declaration") =>
        request.query match {
          case "" =>
            ???
          case s =>
            doDeclaration(s)
        }
      case Some("expression") => // TODO should be POST
        request.query match {
          case "" =>
            ???
          case s =>
            val (xml,compO) = s.indexOf("&component=") match {
              case -1 =>
                ???
              case i => (XMLEscaping.unapply(s.slice("openmath=".length, i)),XMLEscaping.unapply(s.drop(i+1)))
            }
            val comp = compO match {
              case "None" => None
              case s => Some(Path.parseC(s,NamespaceMap.empty))
            }
            doExpression(Obj.parseTerm(XHTML.applyString(xml)(XHTML.Rules.defaultrules).head.node,NamespaceMap.empty),comp)
        }
      case _ => MMTSystem.getResourceAsString("/mmt-web/stex/demo/index.html")
    }
    ServerResponse(ret.toString, "html")
  } catch {
    case t : Throwable =>
      print("")
      throw t
  }

  object Content {
    lazy val th_set = Theory(STeX.set.doc,STeX.set.module.name,None)
    lazy val c_set = Constant(th_set.toTerm,STeX.set.name,Nil,None,None,None,XHTMLTerm.notation("\\set","Set"))
    lazy val th_nat = Theory(STeX.nat.doc,STeX.nat.module.name,None)
    lazy val c_nat = Constant(th_nat.toTerm,STeX.nat.name,Nil,Some(c_set.toTerm),None,None,XHTMLTerm.notation("\\NaturalNumbers","ℕ"))
    lazy val th_prop = Theory(STeX.prop.doc,STeX.prop.module.name,None)
    lazy val c_prop = Constant(th_prop.toTerm,STeX.prop.name,Nil,Some(c_set.toTerm),None,None,XHTMLTerm.notation("\\prop","prop"))
    lazy val th_fun = Theory(STeX.funtype.doc,STeX.funtype.module.name,None)
    lazy val c_fun = Constant(th_fun.toTerm,STeX.funtype.name,Nil,None,None,None,XHTMLTerm.notation("\\funtype{ 1 }","1⟶… prec -9990"))
    lazy val th_impl = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "logic",LocalName("Implication"),None)
    lazy val c_impl = Constant(th_impl.toTerm,LocalName("Implication"),Nil,Some(OMA(c_fun.toTerm,List(c_set.toTerm,c_set.toTerm,c_set.toTerm))),None,None,XHTMLTerm.notation("\\implication{ 1 }{ 2 }","1 ⟹ 2 prec 10"))
    lazy val th_div = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Divisibility"),None)
    lazy val c_even = Constant(th_div.toTerm,LocalName("even"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,c_prop.toTerm))),None,None,XHTMLTerm.notation("\\even{ 1 }","even( 1 ) prec 50"))
    lazy val th_exp = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Exponentiation"),None)
    lazy val c_natexp = Constant(th_exp.toTerm,LocalName("natexp"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,c_nat.toTerm))),None,None,XHTMLTerm.notation("\\natpow{ 1 }{ 2 }","1 ^ 2 prec 70"))

    lazy val th_forall = Theory(STeX.Forall.path.module.parent,STeX.Forall.path.module.name,None)
    lazy val c_forall = Constant(th_forall.toTerm,STeX.Forall.path.name,Nil,None,None,None,XHTMLTerm.notation("\\sforall{ V1 }{ 2 }","∀ V1,… . 2 prec -20"))
    lazy val th_exists = Theory(STeX.Exists.path.module.parent,STeX.Exists.path.module.name,None)
    lazy val c_exists = Constant(th_exists.toTerm,STeX.Exists.path.name,Nil,None,None,None,XHTMLTerm.notation("\\sexists{ V1 }{ 2 }","∃ V1,… . 2 prec -20"))

    lazy val trl_nat = new AcrossLibraryTranslation {
      val tmm = c_nat.toTerm
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case `tmm` => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = MitM.n
    }
    lazy val trl_impl = new AcrossLibraryTranslation {
      val tmm = c_impl.toTerm
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case OMA(`tmm`,List(_,_)) => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val OMA(`tmm`,List(a,b)) = tm
        ApplySpine(OMS(MitM.logic ? "implies"),a,b)
      }
    }
    lazy val trl_even = new AcrossLibraryTranslation {
      val tmm = c_even.toTerm
      val divides = (MitM.basepath / "core" / "arithmetics") ? "NaturalArithmetics" ? "divides"
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case OMA(`tmm`,List(_)) => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val OMA(`tmm`,List(a)) = tm
        ApplySpine(OMS(divides),NatLiterals(2),a)
      }
    }
    lazy val trl_square = new AcrossLibraryTranslation {
      val tmm = c_natexp.toTerm
      val exp = (MitM.basepath / "core" / "arithmetics") ? "NaturalArithmetics" ? "exponentiation"
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case OMA(`tmm`,List(_)) => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val OMA(`tmm`,List(a)) = tm
        ApplySpine(OMS(exp),a,NatLiterals(2))
      }
    }
    lazy val trl_forall = new AcrossLibraryTranslation {
      val tmm = c_forall.toTerm
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case OMBIND(`tmm`,_,_) => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val OMBIND(`tmm`,ctx,a) = tm
        ctx.foldRight(a)((vd,t) => ApplySpine(OMS(MitM.forall),Lambda(vd.name,vd.tp.get,t))) // TODO safer
      }
    }
    lazy val trl_exists = new AcrossLibraryTranslation {
      val tmm = c_exists.toTerm
      override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
        case OMBIND(`tmm`,_,_) => true
        case _ => false
      }
      override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val OMBIND(`tmm`,ctx,a) = tm
        ctx.foldRight(a)((vd,t) => ApplySpine(OMS(MitM.exists),Lambda(vd.name,vd.tp.get,t))) // TODO safer
      }
    }
    lazy val translations = List(trl_nat,trl_impl,trl_even,trl_square,trl_forall,trl_exists)
  }

}
