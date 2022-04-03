package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{NamespaceMap, Path, StructuralElement}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.rules.MathStructureFeature
import info.kwarc.mmt.stex.xhtml.{CustomHTMLNode, HTMLAliasComponent, HTMLArg, HTMLArgMarker, HTMLArityComponent, HTMLAssoctypeComponent, HTMLBindTypeComponent, HTMLComp, HTMLComplexAssignment, HTMLConclusionComponent, HTMLCopyModule, HTMLDefComponent, HTMLDefiniendum, HTMLDomainComponent, HTMLDonotcopy, HTMLFromComponent, HTMLImport, HTMLInputref, HTMLLanguageComponent, HTMLMMTRule, HTMLMacroNameComponent, HTMLMetatheoryComponent, HTMLNotation, HTMLNotationComponent, HTMLNotationFragment, HTMLNotationPrec, HTMLOMA, HTMLOMBIND, HTMLOMID, HTMLOMV, HTMLParser, HTMLRule, HTMLSAssertion, HTMLSDefinition, HTMLSExample, HTMLSParagraph, HTMLSProof, HTMLSProofsketch, HTMLSProofstep, HTMLSignatureComponent, HTMLSimpleAssignment, HTMLSpfcase, HTMLSpfeq, HTMLStatementNameComponent, HTMLStructuralFeature, HTMLSubproof, HTMLSymbol, HTMLTheory, HTMLTheoryHeader, HTMLToComponent, HTMLTopLevelTerm, HTMLTypeComponent, HTMLTypeStringComponent, HTMLUseModule, HTMLVarComp, HTMLVarDecl, HTMLVarSeqDecl, HTMLVarStructDecl, HasHead, MathMLNode, OMDocHTML, SemanticState, SimpleHTMLRule}

object OMDocExtension extends DocumentExtension {

  override def start(args: List[String]): Unit = {
    super.start(args)
    if (!server.extensions.contains(NotationExtractor))
      controller.extman.addExtension(NotationExtractor)
    if (!server.extensions.contains(SymdocRelational)) {
      controller.extman.addExtension(SymdocRelational)
    }
    server.addExtension(classOf[MathStructureFeature])
  }

  object UnknownPropertyRule extends HTMLRule {
    override val priority: Int = -100
    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      if (property(n).exists(_.startsWith("stex:"))) {
        s match {
          case ss : SemanticState =>
            ss.error("Unknown property key: " + property(n).get)
          case _ =>
        }
      }
      None
    }
  }

  case class TermRule(name : String,f : HTMLParser.HTMLNode => OMDocHTML) extends HTMLRule {
    override val priority = 10
    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      if (property(n).contains("stex:" + name)) {
        val ret = f(n)
        s match {
          case state:SemanticState if !state.in_term =>
            Some(HTMLTopLevelTerm(ret))
          case _ => Some(ret)
        }
      } else None
    }
  }

  object MathMLRule extends HTMLRule {
    override val priority = -100

    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      s match {
        case ss: SemanticState if ss.in_term && n.namespace == HTMLParser.ns_mml && n.label != "math" =>
          Some(MathMLNode(n))
        case _ => None
      }
    }
  }

  object FeatureRule extends HTMLRule {
    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      s match {
        case _:SemanticState if property(n).exists(_.startsWith("stex:feature:")) =>
          Some(HTMLStructuralFeature(n,property(n).get.drop(13)))
        case _ => None
      }
    }
  }

  override lazy val rules = List(
    UnknownPropertyRule,
    MathMLRule,
    FeatureRule,
    SimpleHTMLRule("language",HTMLLanguageComponent),
    SimpleHTMLRule("theory",HTMLTheory),
    SimpleHTMLRule("header",HTMLTheoryHeader),
    SimpleHTMLRule("signature",HTMLSignatureComponent),
    SimpleHTMLRule("symdecl",HTMLSymbol),
    SimpleHTMLRule("args",HTMLArityComponent),
    SimpleHTMLRule("macroname",HTMLMacroNameComponent),
    SimpleHTMLRule("assoctype",HTMLAssoctypeComponent),
    SimpleHTMLRule("import",HTMLImport),
    SimpleHTMLRule("usemodule",HTMLUseModule),
    SimpleHTMLRule("copymodule",HTMLCopyModule(_,false)),
    SimpleHTMLRule("interpretmodule",HTMLCopyModule(_,true)),
    SimpleHTMLRule("alias",HTMLAliasComponent),
    SimpleHTMLRule("notation",HTMLNotation),
    SimpleHTMLRule("donotcopy",HTMLDonotcopy),
    SimpleHTMLRule("notationcomp",HTMLNotationComponent),
    SimpleHTMLRule("notationfragment",HTMLNotationFragment),
    SimpleHTMLRule("metatheory",HTMLMetatheoryComponent),
    SimpleHTMLRule("statementname",HTMLStatementNameComponent),
    SimpleHTMLRule("conclusion",HTMLConclusionComponent),
    SimpleHTMLRule("precedence",HTMLNotationPrec),
    SimpleHTMLRule("bindtype",HTMLBindTypeComponent),
    SimpleHTMLRule("domain",HTMLDomainComponent),
    SimpleHTMLRule("assign",HTMLSimpleAssignment),
    SimpleHTMLRule("assignment",HTMLComplexAssignment),
    SimpleHTMLRule("inputref",HTMLInputref),

    SimpleHTMLRule("comp",HTMLComp),
    SimpleHTMLRule("varcomp",HTMLVarComp),
    SimpleHTMLRule("arg",HTMLArg),
    SimpleHTMLRule("argmarker",HTMLArgMarker),
    TermRule("OMBIND",HTMLOMBIND),
    TermRule("OMID",HTMLOMID),
    TermRule("OMA",HTMLOMA),
    TermRule("OMV",HTMLOMV),

    SimpleHTMLRule("definition",HTMLSDefinition),
    SimpleHTMLRule("example",HTMLSExample),
    SimpleHTMLRule("assertion",HTMLSAssertion),
    SimpleHTMLRule("sproof",HTMLSProof),
    SimpleHTMLRule("spfstep",HTMLSProofstep),
    SimpleHTMLRule("proofsketch",HTMLSProofsketch),
    SimpleHTMLRule("subproof",HTMLSubproof),
    SimpleHTMLRule("spfcase",HTMLSpfcase),
    SimpleHTMLRule("spfeq",HTMLSpfeq),
    SimpleHTMLRule("paragraph",HTMLSParagraph),
    SimpleHTMLRule("typestrings",HTMLTypeStringComponent),
    SimpleHTMLRule("definiendum",HTMLDefiniendum),
    SimpleHTMLRule("from",HTMLFromComponent),
    SimpleHTMLRule("to",HTMLToComponent),

    SimpleHTMLRule("vardecl",HTMLVarDecl),
    SimpleHTMLRule("varseq",HTMLVarSeqDecl),
    SimpleHTMLRule("varinstance",HTMLVarStructDecl),
    SimpleHTMLRule("type",HTMLTypeComponent),
    SimpleHTMLRule("definiens",HTMLDefComponent),

    SimpleHTMLRule("mmtrule",HTMLMMTRule)
  )

  import DocumentExtension._
  override lazy val documentRules = List(
    {case thm: HTMLTheory =>
      sidebar(thm, <b style="font-size: larger">Theory: {thm.name.toString}</b> :: Nil)
    },
    {case s: HTMLSymbol =>
      controller.getO(s.path) match {
        case Some(c : Constant) =>
          sidebar(s,{<span style="display:inline">Constant {makeButton("/:" + server.pathPrefix + "/declaration?" + c.path,scala.xml.Text(c.name.toString)
          )}<code>(\{s.macroname})</code></span>} :: Nil)
        case _ =>
      }
    },
    {case t: HasHead if t.isVisible =>
      if (t.resource.startsWith("var://") || t.resource.startsWith("varseq://")) {
        // TODO
      } else {
        overlay(t, "/:" + server.pathPrefix + "/fragment?" + t.head.toString + "&language=" + getLanguage(t),
          "/:" + server.pathPrefix + "/declaration?" + t.head.toString  + "&language=" + getLanguage(t))
      }
    },
  )
/*
  override lazy val documentRules = List(
    {case v: HTMLVariable =>
      val is = List(if (true) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)")) // TODO !
      val seq = scala.xml.Text("Variable ") :: /* server.xhtmlPresenter.asXML(v.path, None) :: */ is // TODO
      sidebar(v, seq)
    },
    {case s: HTMLImport =>
      sidebar(s,{<span style="display:inline">Include {makeButton("/:" + server.pathPrefix + "/theory?" + s.domain,scala.xml.Text(s.name.toString))}</span>} :: Nil)
    },
    /*{case ir : Inputref =>

    },*/
    {case t: HasHeadSymbol =>
      overlay(t,"/:" + server.pathPrefix + "/fragment?" + t.head.toString,"/:" + server.pathPrefix + "/declaration?" + t.head.toString)},
  )

 */

}

object NotationExtractor extends RelationalExtractor with STeXExtension {
  val notation = CustomBinary("notation", "is notation for", "has notation")
  override val allBinary: List[Binary] = List(
    notation
  )

  override val allUnary: List[Unary] = List()
  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t : Theory =>
      t.getConstants.foreach {
        case c if c.rl.contains("notation") =>
          c.tp match {
            case Some(STeX.notation.tp(sym,_)) =>
              f(notation(c.path,sym))
              controller.depstore += notation(c.path,sym)
            case _ =>
              // structures maybe?
          }
        case _ =>
      }
    case _ =>
  }
}

object SymdocRelational extends RelationalExtractor with STeXExtension {
  val documents = CustomBinary("documents","documents","has documentation")
  override val allBinary: List[Binary] = List(
    documents
  )

  override val allUnary: List[Unary] = List()

  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t : Theory =>
      t.getConstants.foreach {
        case c if c.rl.contains("symboldoc") =>
          c.df match {
            case Some(STeX.symboldoc(s,_,_)) =>
              s.foreach { s =>
                val p = Path.parseMS(s, NamespaceMap.empty)
                f(documents(c.path, p))
                controller.depstore += documents(c.path, p)
              }
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
}