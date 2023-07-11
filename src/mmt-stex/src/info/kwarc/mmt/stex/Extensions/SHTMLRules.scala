package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.modules.{AbstractTheory, Theory}
import info.kwarc.mmt.api.objects.OMFOREIGN
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.{DefComponent, LocalName, NamespaceMap, ParseError, Path, StructuralElement}
import info.kwarc.mmt.stex.{SHTML, STeXServer}
import info.kwarc.mmt.stex.rules.{MathStructureFeature, StringLiterals}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLNodeWrapper, HTMLParser, HTMLRule, IsTerm, SHTMLArg, SHTMLArgTypes, SHTMLArgnum, SHTMLAssertion, SHTMLAssignment, SHTMLBind, SHTMLBlindSection, SHTMLComp, SHTMLConclusion, SHTMLDefiniendum, SHTMLDefiniens, SHTMLDefinition, SHTMLDocumentTitle, SHTMLExample, SHTMLFillInSol, SHTMLFrame, SHTMLFrameNumber, SHTMLHeadTerm, SHTMLIfInputref, SHTMLImportModule, SHTMLInputref, SHTMLMCB, SHTMLMCC, SHTMLMCSol, SHTMLMMTRule, SHTMLMMTStructure, SHTMLMathStructure, SHTMLNode, SHTMLNotation, SHTMLNotationComponent, SHTMLOMA, SHTMLOMB, SHTMLOML, SHTMLOMMOD, SHTMLOMS, SHTMLOMStr, SHTMLOMV, SHTMLObjective, SHTMLOpNotationComponent, SHTMLParagraph, SHTMLParsingRule, SHTMLPrecondition, SHTMLPremise, SHTMLProblem, SHTMLProof, SHTMLProofAssumption, SHTMLProofBody, SHTMLProofConclusion, SHTMLProofEqStep, SHTMLProofMethod, SHTMLProofStep, SHTMLProofTerm, SHTMLProofTitle, SHTMLRenaming, SHTMLReturnType, SHTMLRule, SHTMLSCB, SHTMLSCC, SHTMLSCSol, SHTMLSection, SHTMLSectionLevel, SHTMLSectionTitle, SHTMLSolution, SHTMLState, SHTMLSubProof, SHTMLSymbol, SHTMLTheory, SHTMLTitle, SHTMLType, SHTMLUseModule, SHTMLVarComp, SHTMLVarNotation, SHTMLVardef, SHTMLVarseq, SHTMLVisible, SemanticState, TopLevelTerm}

trait OMDocSHTMLRules { this : STeXServer =>

  object UnknownPropertyRule extends SHTMLRule(-100) {
    def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = if (attrs.nonEmpty) {
      println("Missing: " + attrs.map(_._1).mkString(", "))
      None
    } else None
  }

  lazy val importRules = List(
    //UnknownPropertyRule,
    SHTMLParsingRule("multiple-choice-block", (_, n, _) => SHTMLMCB(n)),
    SHTMLParsingRule("mcc", (_, n, _) => SHTMLMCC(n)),
    SHTMLParsingRule("mcc-solution", (_, n, _) => SHTMLMCSol(n)),
    SHTMLParsingRule("single-choice-block", (_, n, _) => SHTMLSCB(n)),
    SHTMLParsingRule("scc", (_, n, _) => SHTMLSCC(n)),
    SHTMLParsingRule("scc-solution", (_, n, _) => SHTMLSCSol(n)),
    SHTMLParsingRule("fillinsol", (_, n, _) => SHTMLFillInSol(n)),
    SHTMLParsingRule("solution", (_, n, _) => SHTMLSolution(n)),
    SHTMLParsingRule("problem", (str, n, _) => {
      val mp = Path.parseM(str)
      new SHTMLProblem(mp, n)
    }),

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
    SHTMLParsingRule("maincomp", (s, n, _) => try {
      val gn = Path.parseS(s)
      SHTMLComp(gn, n)
    } catch {
      case _:ParseError => SHTMLVarComp(LocalName.parse(s), n)
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
    SHTMLParsingRule("premise", (s, n, _) => SHTMLPremise(n), -10),
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
    SHTMLParsingRule("preconditionsymbol", (_, n, _) => SHTMLPrecondition(n)),
    SHTMLParsingRule("objectivesymbol", (_, n, _) => SHTMLObjective(n)),
    SHTMLParsingRule("OMBIND", (s, n, _) => SHTMLOMB(n)),
    SHTMLParsingRule("OMA", (s, n, _) => SHTMLOMA(n)),
    SHTMLParsingRule("OMID", (s, n, _) => SHTMLOMS(n)),
    SHTMLParsingRule("OMV", (s, n, _) => SHTMLOMV(n)),
    SHTMLParsingRule("arg", (s, n, _) => SHTMLArg(s.toInt, n), -10),
    SHTMLParsingRule("notationcomp", (s, n, _) => SHTMLNotationComponent(n)),
    SHTMLParsingRule("notationopcomp", (s, n, _) => SHTMLOpNotationComponent(n)),
    SHTMLParsingRule("argnum", (s, n, _) => SHTMLArgnum(s.head.toInt, n)),
    new SHTMLRule(-50) {
      def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        attrs.find(_._1 == "srefin") match {
          case Some((_,p)) =>
            val f = File(p.replace(".sref",".tex"))
            controller.backend.resolvePhysical(f) match {
              case Some((a,fp)) =>
                n.plain.attributes((HTMLParser.ns_shtml,"srefin")) = a.id + "::" + fp.mkString("/")
              case _ =>
            }
          case _ =>
        }
        None
      }
    },
    new SHTMLRule(-50) {
      override def apply(state: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        if (n.label == "a") {
          n.plain.attributes.get((HTMLParser.ns_html, "id")) match {
            case Some(s) if s.startsWith("sref@") =>
              state match {
                case state : SemanticState =>
                  val id = s.hashCode.toHexString
                  SHTMLContentManagement.addSref(state.doc,id)
                case _ =>
              }
            case _ =>
          }
        }
        None
      }
    },
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
