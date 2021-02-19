package info.kwarc.mmt.stex.demo

import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{ContentPath, DPath, DefComponent, GlobalName, LocalName, NamespaceMap, Path}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.objects.{OMA, OMBIND, OMS, Term, VarDecl}
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, AlignmentTranslation, LinkTranslation, TranslationGroup, TranslationTarget}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, MMTSystem, URI}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Typed}
import info.kwarc.mmt.odk.{LFX, NatLiterals}
import info.kwarc.mmt.stex.{InformalMathMLPresenter, STeX}
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, XHTML, XHTMLDecl, XHTMLNode, XHTMLOMV, XHTMLSidebar, XHTMLTerm, XHTMLTheorem, XHTMLTheory, XHTMLVarDecl}

import scala.xml.NodeSeq

class DemoServer extends ServerExtension("stexdemo") {
  private var initialized = false
  object Content {
    lazy val th_nat = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("NaturalNumbers"),None)
    lazy val c_nat = Constant(th_nat.toTerm,LocalName("NaturalNumbers"),Nil,None,None,None,XHTMLTerm.notation("ℕ"))
    lazy val th_logic = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "logic",LocalName("Implication"),None)
    lazy val c_impl = Constant(th_logic.toTerm,LocalName("Implication"),Nil,None,None,None,XHTMLTerm.notation("1 ⟹ 2 prec 10"))
    lazy val th_div = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Divisibility"),None)
    lazy val c_even = Constant(th_div.toTerm,LocalName("even"),Nil,None,None,None,XHTMLTerm.notation("even( 1 ) prec 50"))
    lazy val th_exp = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Exponentiation"),None)
    lazy val c_square = Constant(th_exp.toTerm,LocalName("square"),Nil,None,None,None,XHTMLTerm.notation("1 ² prec 70"))
    lazy val th_forall = Theory(STeX.Forall.path.module.parent,STeX.Forall.path.module.name,None)
    lazy val c_forall = Constant(th_forall.toTerm,STeX.Forall.path.name,Nil,None,None,None,XHTMLTerm.notation("∀ V1,… . 2 prec -20"))
    lazy val th_exists = Theory(STeX.Exists.path.module.parent,STeX.Exists.path.module.name,None)
    lazy val c_exists = Constant(th_exists.toTerm,STeX.Exists.path.name,Nil,None,None,None,XHTMLTerm.notation("∃ V1,… . 2 prec -20"))

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
      val tmm = c_square.toTerm
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
  def initialize = if (!initialized) {
    import Content._
    controller.add(th_nat)
    controller.add(c_nat)
    controller.add(th_logic)
    controller.add(c_impl)
    controller.add(th_div)
    controller.add(c_even)
    controller.add(th_exp)
    controller.add(c_square)
    controller.add(th_forall)
    controller.add(c_forall)
    controller.add(th_exists)
    controller.add(c_exists)
  }

  lazy val mitm_translator = new AcrossLibraryTranslator(controller,Content.translations,Nil,((path: GlobalName, _: Controller) =>
    Typed._base <= path ||
    LFX.ns <= path ||
    MitM.basepath <= path),false)

  lazy val presenter = controller.extman.get(classOf[InformalMathMLPresenter]) match {
    case p :: _ => p
    case Nil =>
      val p = new InformalMathMLPresenter
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

    head.add(<style>{scala.xml.Text(""".stexoverlay {
                                      |  position:absolute;
                                      |  top:2em;
                                      |  opacity:0;
                                      |  width:0%;
                                      |  height:0%;
                                      |  transition: .5s ease;
                                      |}
                                      |.stexoverlaycontainer:hover {
                                      |  background-color:yellow;
                                      |}
                                      |span.stexoverlaycontainer:hover + span.stexoverlay {
                                      |  opacity: 1;
                                      |  width:80ch;
                                      |  height:50%;
                                      |}""".stripMargin)}</style>)
  }

  def getFragment(s:String) : String = MMTSystem.getResourceAsString("mmt-web" + (s match {
    case _ if s == Content.c_nat.path.toString =>
      "/stex/demo/naturalnumbers.en.xhtml"
    case _ if s == Content.c_impl.path.toString =>
      "/stex/demo/implication.en.xhtml"
    case _ if s == Content.c_even.path.toString =>
      "/stex/demo/even.en.xhtml"
    case _ if s == Content.c_square.path.toString =>
      "/stex/demo/square.en.xhtml"
    case _ => "missing"
  }))

  def doFragment(uri : String) = {
    import info.kwarc.mmt.stex.xhtml.XHTML.Rules._
    val filecontent = XHTML.applyString(getFragment(uri)).head
    doHeader(filecontent)
    val body = filecontent.get("body")().head
    body.attributes(("","style")) = "margin:0;padding:0;"
    val doc = body.get("div")(("","class","ltx_page_main")).head
    doc.attributes(("","style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
    doc.get("div")().foreach {e =>
      if (e.attributes.get(("","class")).exists(_.contains("ltx_theorem"))) {
        e.attributes(("","style")) = "margin:0;"
      }
    }
    ServerResponse(filecontent.toString, "html")
  }

  def getDocument(uri : String) : String = uri match {
    case "http://mathhub.info/fomtex/demo.xhtml" =>
      MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml")
    case _ => ???
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
        val sb = List(scala.xml.Text("Theorem " + thm.name.toString + "\n"), presenter.asXML(c.df.get, Some(c.path $ DefComponent)))
        thm.add(XHTMLSidebar(thm.path.toString, sb: _*), None)
      case v: XHTMLVarDecl =>
        controller add v.toDeclaration
        val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
        val seq = scala.xml.Text("Variable ") :: presenter.asXML(v.vardecl, None) :: is
        v.parent.foreach(_.add(XHTMLSidebar(v.name.toString, seq: _*), Some(v)))
      case _ =>
    }
    filecontent.iterate {
      case t: HasHeadSymbol =>
        t.addOverlay("/:" + this.pathPrefix + "/fragment?" + t.head.toString)
      case v : XHTMLOMV =>
        v.addOverlay("/:" + this.pathPrefix + "/declaration?" + v.path)
      case _ =>
    }
    ServerResponse(filecontent.toString, "html")
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
    val content = c.rl match {
      case Some("variable") =>
        "Variable: " + presenter.asXML(VarDecl(c.name,None,c.tp,None,c.not),None) + {
          c.metadata.getValues(STeX.meta_quantification) match {
            case List(OMS(STeX.Forall.path)) => " (universally quantified)"
            case List(OMS(STeX.Exists.path)) => " (existentially quantified)"
            case _ => ""
          }
        }
      case _ =>
        ???
    }
    val footer = "</body>\n</html>"
    ServerResponse(header + content + footer, "html")
  }

  override def apply(request: ServerRequest): ServerResponse = try {
    initialize
    request.path.lastOption match {
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
      case _ => ServerResponse(MMTSystem.getResourceAsString("/mmt-web/stex/demo/index.html"),"html")
    }
  } catch {
    case t : Throwable =>
      print("")
      throw t
  }


}
