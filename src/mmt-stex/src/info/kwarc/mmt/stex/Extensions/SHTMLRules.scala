package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.modules.{AbstractTheory, Theory}
import info.kwarc.mmt.api.objects.OMFOREIGN
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule}
import info.kwarc.mmt.api.{DefComponent, LocalName, NamespaceMap, Path, StructuralElement}
import info.kwarc.mmt.stex.STeXServer
import info.kwarc.mmt.stex.rules.MathStructureFeature
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLNodeWrapper, HTMLParser, HTMLRule, IsTerm, SHTMLArg, SHTMLArgTypes, SHTMLArgnum, SHTMLAssertion, SHTMLAssignment, SHTMLBind, SHTMLBlindSection, SHTMLComp, SHTMLConclusion, SHTMLDefiniendum, SHTMLDefiniens, SHTMLDefinition, SHTMLDocumentTitle, SHTMLExample, SHTMLFillInSol, SHTMLFrame, SHTMLFrameNumber, SHTMLHeadTerm, SHTMLIfInputref, SHTMLImportModule, SHTMLInputref, SHTMLMCB, SHTMLMCC, SHTMLMCSol, SHTMLMMTRule, SHTMLMMTStructure, SHTMLMathStructure, SHTMLNode, SHTMLNotation, SHTMLNotationComponent, SHTMLOMA, SHTMLOMB, SHTMLOML, SHTMLOMMOD, SHTMLOMS, SHTMLOMStr, SHTMLOMV, SHTMLOpNotationComponent, SHTMLParagraph, SHTMLParsingRule, SHTMLProof, SHTMLProofAssumption, SHTMLProofBody, SHTMLProofConclusion, SHTMLProofEqStep, SHTMLProofMethod, SHTMLProofStep, SHTMLProofTerm, SHTMLProofTitle, SHTMLRenaming, SHTMLReturnType, SHTMLRule, SHTMLSection, SHTMLSectionLevel, SHTMLSectionTitle, SHTMLSolution, SHTMLState, SHTMLSubProof, SHTMLSymbol, SHTMLTheory, SHTMLTitle, SHTMLType, SHTMLUseModule, SHTMLVarComp, SHTMLVarNotation, SHTMLVardef, SHTMLVarseq, SHTMLVisible, TopLevelTerm}

trait OMDocSHTMLRules { this : STeXServer =>

  object UnknownPropertyRule extends SHTMLRule(-100) {
    def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = if (attrs.nonEmpty) {
      println("Missing: " + attrs.map(_._1).mkString(", "))
      None
    } else None
  }

  lazy val importRules = List(
    UnknownPropertyRule,
    SHTMLParsingRule("multiple-choice-block", (_, n, _) => SHTMLMCB(n)),
    SHTMLParsingRule("mcc", (_, n, _) => SHTMLMCC(n)),
    SHTMLParsingRule("mcc-solution", (_, n, _) => SHTMLMCSol(n)),
    SHTMLParsingRule("fillinsol", (_, n, _) => SHTMLFillInSol(n)),
    SHTMLParsingRule("solution", (_, n, _) => SHTMLSolution(n)),
    SHTMLParsingRule("proof",(_,n,_) => SHTMLProof(n)),
    SHTMLParsingRule("prooftitle", (_, n, _) => SHTMLProofTitle(n)),
    SHTMLParsingRule("proofbody", (_, n, _) => SHTMLProofBody(n)),
    SHTMLParsingRule("proofterm", (_, n, _) => SHTMLProofTerm(n)),
    SHTMLParsingRule("subproof", (_, n, _) => SHTMLSubProof(n)),
    SHTMLParsingRule("spfassumption", (_, n, _) => SHTMLProofAssumption(n)),
    SHTMLParsingRule("spfstep", (_, n, _) => SHTMLProofStep(n)),
    SHTMLParsingRule("spfeqstep", (_, n, _) => SHTMLProofEqStep(n)),
    SHTMLParsingRule("spfconclusion", (_, n, _) => SHTMLProofConclusion(n)),
    SHTMLParsingRule("proofmethod", (_, n, _) => SHTMLProofMethod(n)),
    SHTMLParsingRule("argtypes", (_,n,_) => SHTMLArgTypes(n)),
    SHTMLParsingRule("bind", (str, n, _) => SHTMLBind(LocalName.parse(str),n)),
    SHTMLParsingRule("visible", (str, n, _) => SHTMLVisible(str != "false", n), 100),
    SHTMLParsingRule("frame", (str, n, _) => new SHTMLFrame(n)),
    SHTMLParsingRule("section",(_,n,_) => SHTMLSection(n)),
    SHTMLParsingRule("skipsection", (_, n, _) => SHTMLBlindSection(n)),
    SHTMLParsingRule("sectiontitle", (_, n, _) => SHTMLSectionTitle(n)),
    SHTMLParsingRule("sectionlevel",(s,n,_) => SHTMLSectionLevel(s.toInt,n)),
    SHTMLParsingRule("framenumber",(_,n,_) => SHTMLFrameNumber(n)),
    SHTMLParsingRule("theory", (str, n, _) => {
      val mp = Path.parseM(str)
      new SHTMLTheory(mp, n)
    }),
    SHTMLParsingRule("problem", (str, n, _) => {
      val mp = Path.parseM(str)
      new SHTMLTheory(mp, n)
    }),
    SHTMLParsingRule("feature-morphism", (str, n, _) => {
      val gn = Path.parseS(str)
      SHTMLMMTStructure(gn,n)
    }),
    SHTMLParsingRule("rename", (str, n, _) => {
      val gn = Path.parseS(str)
      SHTMLRenaming(gn, n)
    }),
    SHTMLParsingRule("assign", (str, n, _) => {
      val gn = Path.parseS(str)
      SHTMLAssignment(gn, n)
    }),
    SHTMLParsingRule("feature-structure", (s, n, _) => {
      val mp = Path.parseM(s)
      new SHTMLMathStructure(mp, n)
    }),
    SHTMLParsingRule("symdecl", (str, n, _) => {
      val gn = Path.parseS(str)
      new SHTMLSymbol(gn, n)
    }),
    SHTMLParsingRule("import", (str, n, _) => {
      val mp = Path.parseM(str)
      SHTMLImportModule(mp, n)
    }),
    SHTMLParsingRule("usemodule", (str, n, _) => {
      val mp = Path.parseM(str)
      SHTMLUseModule(mp, n)
    }),
    SHTMLParsingRule("notation", (str, n, _) => {
      if (str.split('?').length == 3) {
        val gn = Path.parseS(str)
        SHTMLNotation(gn, n)
      } else SHTMLVarNotation(LocalName.parse(str),n)
    }),
    SHTMLParsingRule("rule", (str, n, _) => {
      val mp = Path.parseM(str)
      SHTMLMMTRule(mp, n)
    }),
    SHTMLParsingRule("vardef", (str, n, _) => {
      new SHTMLVardef(LocalName.parse(str), n)
    }),
    SHTMLParsingRule("varseq", (str, n, _) => {
      new SHTMLVarseq(LocalName.parse(str), n)
    }),
    SHTMLParsingRule("comp", (s, n, _) => {
      val gn = Path.parseS(s)
      SHTMLComp(gn, n)
    }, 10),
    SHTMLParsingRule("definiendum", (s, n, _) => {
      val gn = Path.parseS(s)
      SHTMLDefiniendum(gn, n)
    }, 10),
    SHTMLParsingRule("varcomp", (s, n, _) => {
      val gn = LocalName.parse(s)
      SHTMLVarComp(gn, n)
    }, 10),
    SHTMLParsingRule("ifinputref", (_,n,_) => SHTMLIfInputref(n)),
    SHTMLParsingRule("headterm", (_, n, _) => SHTMLHeadTerm(n), -20),
    SHTMLParsingRule("inputref", (s, n, _) => SHTMLInputref(s, n)),
    SHTMLParsingRule("doctitle", (_, n, _) => SHTMLDocumentTitle(n)),
    SHTMLParsingRule("paragraph", (_, n, _) => SHTMLParagraph(n), -30),
    SHTMLParsingRule("example", (_, n, _) => SHTMLExample(n), -30),
    SHTMLParsingRule("definition", (_, n, _) => SHTMLDefinition(n), -30),
    SHTMLParsingRule("assertion", (_, n, _) => SHTMLAssertion(n), -30),
    SHTMLParsingRule("type", (s, n, _) => SHTMLType(n),-10),
    SHTMLParsingRule("statementtitle", (_,n,_) => SHTMLTitle(n),-20),
    SHTMLParsingRule("returntype", (s, n, _) => SHTMLReturnType(n),-10),
    SHTMLParsingRule("definiens", (s, n, _) => SHTMLDefiniens(n), -10),
    SHTMLParsingRule("conclusion", (s, n, _) => SHTMLConclusion(n), -10),
    SHTMLParsingRule("omstr", (_,n,_) => SHTMLOMStr(n)),
    SHTMLParsingRule("term", (s, n, _) => {
      s match {
        case "OMID" | "complex" => SHTMLOMS(n)
        case "OMA" => SHTMLOMA(n)
        case "OMV" => SHTMLOMV(n)
        case "OMBIND" => SHTMLOMB(n)
        case "OMMOD" => SHTMLOMMOD(n)
        case "OML" => SHTMLOML(n)
        case _ =>
          ???
      }
    }),
    SHTMLParsingRule("OMBIND", (s, n, _) => SHTMLOMB(n)),
    SHTMLParsingRule("OMA", (s, n, _) => SHTMLOMA(n)),
    SHTMLParsingRule("OMID", (s, n, _) => SHTMLOMS(n)),
    SHTMLParsingRule("OMV", (s, n, _) => SHTMLOMV(n)),
    SHTMLParsingRule("arg", (s, n, _) => SHTMLArg(s.toInt, n), -10),
    SHTMLParsingRule("notationcomp", (s, n, _) => SHTMLNotationComponent(n)),
    SHTMLParsingRule("notationopcomp", (s, n, _) => SHTMLOpNotationComponent(n)),
    SHTMLParsingRule("argnum", (s, n, _) => SHTMLArgnum(s.head.toInt, n)),
    new SHTMLRule(-50) {
      def rec(nn: HTMLNode, s: HTMLParser.ParsingState, top: HTMLNode): Option[SHTMLNode] = (nn, s) match {
        case (_: IsTerm, s: SHTMLState[SHTMLNode]) if !s.in_term =>
          Some(TopLevelTerm(top))
        case (nn: SHTMLNode, _: SHTMLState[SHTMLNode]) =>
          rec(nn.inner, s, top)
        case _ => None
      }

      def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        rec(n, s, n)
      }
    }
  )

}
/*
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


/*
  case class TermRule(name : String,f : HTMLParser.HTMLNode => OMDocHTML) extends HTMLRule {
    override val priority = 10
    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      if (property(n).contains("shtml:" + name)) {
        val ret = f(n)
        s match {
          case state:SemanticState if !state.in_term =>
            Some(new HTMLTopLevelTerm(ret))
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
    override val priority: Int = -50
    override def apply(s: HTMLParser.ParsingState, n: HTMLParser.HTMLNode): Option[HTMLParser.HTMLNode] = {
      s match {
        case _:SemanticState if property(n).exists(_.startsWith("shtml_feature:")) =>
          Some(HTMLStructuralFeature(n,property(n).get.drop(13)))
        case _ => None
      }
    }
  }

 */

  /*override lazy val rules = List(UnknownPropertyRule
    UnknownPropertyRule,
    MathMLRule,
    FeatureRule,
    SimpleHTMLRule("feature:structure",HTMLStructureFeature),
    SimpleHTMLRule("language",HTMLLanguageComponent),
    SimpleHTMLRule("theory",HTMLTheory),
    SimpleHTMLRule("problem",HTMLProblem),
    SimpleHTMLRule("header",HTMLTheoryHeader),
    SimpleHTMLRule("signature",HTMLSignatureComponent),
    SimpleHTMLRule("symdecl",HTMLSymbol),
    SimpleHTMLRule("args",HTMLArityComponent),
    SimpleHTMLRule("macroname",HTMLMacroNameComponent),
    SimpleHTMLRule("assoctype",HTMLAssoctypeComponent),
    SimpleHTMLRule("reorderargs",HTMLReorderComponent),
    SimpleHTMLRule("doctitle",HTMLDoctitle),
    SimpleHTMLRule("import",HTMLImport),
    SimpleHTMLRule("usemodule",HTMLUseModule),
    SimpleHTMLRule("copymodule",HTMLCopyModule(_,false)),
    SimpleHTMLRule("interpretmodule",HTMLCopyModule(_,true)),
    SimpleHTMLRule("realize",HTMLRealization),
    SimpleHTMLRule("alias",HTMLAliasComponent),
    SimpleHTMLRule("notation",HTMLNotation),
    SimpleHTMLRule("donotcopy",HTMLDonotcopy),
    SimpleHTMLRule("notationcomp",HTMLNotationComponent),
    SimpleHTMLRule("notationopcomp",HTMLNotationOpComponent),
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
    SimpleHTMLRule("includeproblem",HTMLIncludeproblem),
    SimpleHTMLRule("solution",HTMLSolution),

    SimpleHTMLRule("comp",HTMLComp),
    SimpleHTMLRule("varcomp",HTMLVarComp),
    SimpleHTMLRule("arg",HTMLArg),
    SimpleHTMLRule("argmarker",HTMLArgMarker),
    TermRule("OMBIND",HTMLOMBIND),
    TermRule("OMID",HTMLOMID),
    TermRule("OMA",HTMLOMA),
    TermRule("OMV",HTMLOMV),

    SimpleHTMLRule("frame",HTMLFrame),

    SimpleHTMLRule("definition",HTMLSDefinition),
    SimpleHTMLRule("example",HTMLSExample),
    SimpleHTMLRule("assertion",HTMLSAssertion),
    SimpleHTMLRule("judgment",HTMLJudgment),
    SimpleHTMLRule("sproof",HTMLSProof),
    SimpleHTMLRule("spfstep",HTMLSProofstep),
    SimpleHTMLRule("spfyield",HTMLSProofyield),
    SimpleHTMLRule("spftitle",HTMLSProoftitle),
    SimpleHTMLRule("spfbody",HTMLSProofbody),
    SimpleHTMLRule("proofsketch",HTMLSProofsketch),
    SimpleHTMLRule("subproof",HTMLSubproof),
    SimpleHTMLRule("spfcase",HTMLSpfcase),
    SimpleHTMLRule("spfeq",HTMLSpfeq),
    SimpleHTMLRule("paragraph",HTMLSParagraph),
    SimpleHTMLRule("typestrings",HTMLTypeStringComponent),
    SimpleHTMLRule("definiendum",HTMLDefiniendum),
    SimpleHTMLRule("from",HTMLFromComponent),
    SimpleHTMLRule("to",HTMLToComponent),
    SimpleHTMLRule("premise",HTMLPremise),

    SimpleHTMLRule("vardecl",HTMLVarDecl),
    SimpleHTMLRule("varseq",HTMLVarSeqDecl),
    SimpleHTMLRule("startindex",HTMLVarSeqStart),
    SimpleHTMLRule("endindex",HTMLVarSeqEnd),
    SimpleHTMLRule("varinstance",HTMLVarStructDecl),
    SimpleHTMLRule("type",HTMLTypeComponent),
    SimpleHTMLRule("definiens",HTMLDefComponent),

    SimpleHTMLRule("mmtrule",HTMLMMTRule)
  )
      */

  import DocumentExtension._

}

 */
