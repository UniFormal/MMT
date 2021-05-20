package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Context.context2list
import info.kwarc.mmt.api.objects.{OMA, OMAorAny, OMID, OMMOD, OMV, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, Declaration, DerivedDeclaration, FinalConstant, RuleConstant, RuleConstantInterpreter}
import info.kwarc.mmt.api.uom.AbbrevRule
import info.kwarc.mmt.api.{LocalName, MPath, Path, StructuralElement}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.xhtml.{HTMLArity, HTMLConstant, HTMLDefiniens, HTMLDerived, HTMLImport, HTMLMMTRule, HTMLMacroname, HTMLNotation, HTMLNotationComponent, HTMLNotationFragment, HTMLNotationPrec, HTMLOMA, HTMLOMBIND, HTMLOMID, HTMLParser, HTMLRule, HTMLSymbol, HTMLTheory, HTMLTheoryHeader, HTMLType, HTMLUseModule, HasHeadSymbol, HasNotation, HasTermArgs, LanguageComponent, MathMLLiteral, MathMLTerm, MetatheoryComponent, OMDocParent, SemanticState, SignatureComponent}

object OMDocExtension extends DocumentExtension {
/*
  override val checkingRules = List(
    {case (c : Constant,s) if c.rl.contains("notation") =>
      controller add c
      c
    }
  )

 */


  override def start(args: List[String]): Unit = {
    super.start(args)
    if (!server.extensions.contains(NotationExtractor))
      controller.extman.addExtension(NotationExtractor)
  }

  class HTMLVariable(orig : HTMLParser.HTMLNode) extends HTMLConstant(orig) with HasTermArgs {
    def makeVariable(state : SemanticState, ppath : MPath, parent : OMDocParent) = {
      val cs = getArgs.flatMap { arg =>
        val ctx = context2list(state.makeBinder(arg))
        ctx.map {vd =>
          val c = Constant(OMMOD(ppath),parent.newName(resource),Nil,vd.tp,vd.df,Some("variable"))
          SourceRef.get(arg).foreach(s => SourceRef.update(c,s))
          c.metadata.update(STeX.meta_vardecl,STeX.StringLiterals(resource))
          state.controller.add(c)
          state.check(c)
          val orig = vd.metadata.getValues(STeX.meta_notation).head.asInstanceOf[Term]
          controller.getO(ppath) match {
            case Some(t : Theory) =>
              t.paramC.free = t.paramC.free ++ VarDecl(c.name,None,vd.tp,vd.df,None)
            case Some(d : DerivedDeclaration) =>
              // TODO needs extending innerContext/parameters in [[DerivedContentElement]]
            case _ =>
              print("")
          }
          parent.addRule({case `orig` =>
            val omv = OMV(c.name)
            omv.metadata.update(STeX.meta_source,c.toTerm)
            omv
          })
          c
        }
      }
      attributes((HTMLParser.ns_stex,"constant")) = cs.map(_.path).mkString(" ")
    }
    override def onAdd: Unit = sstate.foreach { state =>
      collectAncestor {
        case t : HTMLTheory =>
          t.signature_theory.toList.foreach { th =>
            makeVariable(state,th.path,t)
          }
        case p : HTMLDerived =>
          makeVariable(state,p.path,p)
        case p : OMDocParent =>
          print("")
      }
    }
  }

  class HTMLTermConstant(orig : HTMLParser.HTMLNode) extends HTMLConstant(orig) with HasTermArgs {
    def makeConstant(state : SemanticState, ppath : MPath, parent : OMDocParent) = {
      val cs = getArgs.map { arg =>
        val c = Constant(OMMOD(ppath),parent.newName(resource),Nil,None,Some(state.applyTerm(arg)),Some(resource))
        SourceRef.get(arg).foreach(s => SourceRef.update(c,s))
        state.controller.add(c)
        state.check(c)
        c
      }
      attributes((HTMLParser.ns_stex,"constant")) = cs.map(_.path).mkString(" ")
    }
    override def onAdd: Unit = sstate.foreach { state =>
      collectAncestor {
        case t : HTMLTheory =>
          t.signature_theory.toList.foreach { th =>
            makeConstant(state,th.path,t)
          }
        case p : HTMLDerived =>
          makeConstant(state,p.path,p)
        case p : OMDocParent =>
          print("")
      }
    }
  }

  override lazy val rules = List(
    new HTMLRule {
      override def rule(s: HTMLParser.ParsingState): PartialFunction[HTMLParser.HTMLNode, HTMLParser.HTMLNode] = {
            // Declarations
        case n if property(n).contains("stex:theory") => new HTMLTheory(n)
        case n if property(n).contains("stex:symdecl") => new HTMLSymbol(n)
        case n if property(n).contains("stex:notation") => new HTMLNotation(n)
        case n if property(n).contains("stex:import") => new HTMLImport(n)
        case n if property(n).contains("stex:usemodule") => new HTMLUseModule(n)
        case n if property(n).exists(_.startsWith("stex:feature:")) => new HTMLDerived(n)
        case n if property(n).contains("stex:mmtrule") => new HTMLMMTRule(n)
        case n if property(n).contains("stex:term") => new HTMLTermConstant(n)
        case n if property(n).contains("stex:variable") => new HTMLVariable(n)
            // Terms
        case n if property(n).contains("stex:OMID") => new HTMLOMID(n)
        case n if property(n).contains("stex:OMA") => new HTMLOMA(n)
        case n if property(n).contains("stex:OMBIND") => new HTMLOMBIND(n)
            // MathML
        case n if n.namespace == HTMLParser.ns_mml && n.label == "mn" => new MathMLLiteral(n)
        case n if n.namespace == HTMLParser.ns_mml => new MathMLTerm(n)
            // Components
        case n if property(n).contains("stex:args") => new HTMLArity(n)
        case n if property(n).contains("stex:header") => new HTMLTheoryHeader(n)
        case n if property(n).contains("stex:macroname") => new HTMLMacroname(n)
        case n if property(n).contains("stex:language") => new LanguageComponent(n)
        case n if property(n).contains("stex:signature") => new SignatureComponent(n)
        case n if property(n).contains("stex:notationcomp") => new HTMLNotationComponent(n)
        case n if property(n).contains("stex:notationfragment") => new HTMLNotationFragment(n)
        case n if property(n).contains("stex:precedence") => new HTMLNotationPrec(n)
        case n if property(n).contains("stex:metatheory") => new MetatheoryComponent(n)
        case n if property(n).contains("stex:type") => new HTMLType(n)
        case n if property(n).contains("stex:definiens") => new HTMLDefiniens(n)
      }
    }
  )


  /*
  class LanguageComponent(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
    print("")
    def language = resource

    override def open(state: SemanticParsingState): Unit = {
      state.getParent match {
        case t : TheoryAnnotation if language != "" =>
          if (language != "" && t.signaturemodule.exists(s => s.language != "" && s.language != language)) {
            t.signaturemodule = None
          }
          t.languagemodule = Some({
            val nt = new PreTheory(t.node,t.path.toDPath ? language)
            nt.language = language
            nt.add(this)
            val n = new ImportAnnotation(node) {
              override lazy val domain: MPath = t.path
              _parent = Some(nt)
            }
            node.deleteAnnotation(n)
            nt.add(n)
            nt
          })
        case t : HasLanguage =>
          super.open(state)
          t.language = language
        case _ =>
          super.open(state)
      }
    }
  }
  class SignatureComponent(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
    def sig = resource
    override def open(state: SemanticParsingState): Unit = {
      super.open(state)
      _parent match {
        case Some(t : TheoryAnnotation) if sig != "" =>
          if (t.languagemodule.exists(s => s.language != "" && s.language != sig)) {
            t.signaturemodule = None
          } else {
            t.signaturemodule = Some({
              val nt = new PreTheory(t.node,t.path)
              nt.language = sig
              nt
            })
          }
        case _ =>
      }
    }
  }

  class ImportAnnotation(node : XHTMLNode) extends StructureAnnotation(node) with Plain {
    override def open(state: SemanticParsingState): Unit = state.getParent match {
      case p : TheoryAnnotation =>
        p.signaturemodule.foreach { p =>
          _parent = Some(p)
          p.add(this)
        }
      case _ =>
        super.open(state)
    }

    override def close(state: SemanticParsingState): Unit = state.getParent match {
      case p : MetatheoryComponent =>
        p._metatheory = resource
      case _ =>
        super.close(state)
    }
  }

  class UseModuleAnnotation(node : XHTMLNode) extends StructureAnnotation(node) with Plain {
    override def open(state: SemanticParsingState): Unit = state.getParent match {
      case p : TheoryAnnotation =>
        p.languagemodule.foreach { p =>
          _parent = Some(p)
          p.add(this)
        }
      case _ =>
        super.open(state)
    }
  }

  /*
  class RuleAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with ToScript {
    // TODO
  }

 */

  class SymdeclAnnotation(node : XHTMLNode) extends ConstantAnnotation(node) with ToScript {
    override def open(state: SemanticParsingState): Unit = state.getParent match {
      case p : TheoryAnnotation if p.signaturemodule.isDefined =>
        p.signaturemodule.foreach { p =>
          p.add(this)
          _parent = Some(p)
        }
      case p : TheoryAnnotation =>
        p.languagemodule.foreach { p =>
          p.add(this)
          _parent = Some(p)
        }
      case p : PreParent =>
        p.add(this)
        _parent = Some(p)
      case _ =>
    }

    /* override def getElement(implicit state: SemanticParsingState): List[Declaration] = {
      val ls = super.getElement
      if (_definientia.nonEmpty) {
          ls.head.asInstanceOf[FinalConstant].df.map {df =>
            ls ::: RuleConstant(ls.head.home,_parent.get.newName("abbrevrule"),OMA(OMID(Path.parseM("scala://features.stex.mmt.kwarc.info?AbbreviationRule")),
              List(ls.head.toTerm,df)),Some(new AbbrevRule(ls.head.path,df))) :: Nil
          }.getOrElse(ls)
      } else ls
    } */
  }

  class NotationAnnotation(node : XHTMLNode) extends ConstantAnnotation(node) with ToScript {
    override def open(state: SemanticParsingState): Unit = state.getParent match {
      case p: TheoryAnnotation if p.signaturemodule.isDefined =>
        p.signaturemodule.foreach { p =>
          p.add(this)
          _parent = Some(p)
        }
      case p: TheoryAnnotation =>
        p.languagemodule.foreach { p =>
          p.add(this)
          _parent = Some(p)
        }
      case p: PreParent =>
        p.add(this)
        _parent = Some(p)
      case _ =>
    }

    override def getElement(implicit state: SemanticParsingState): List[FinalConstant] = _parent match {
      case Some(p: PreTheory) =>
        List(Constant(OMID(p.path), _parent.get.newName("notation"), Nil,
          Some(STeX.notation.tp(Path.parseS(resource), arity)),
          Some(STeX.notation(_notations.head._3, precedence)), Some("notation")))
      case _ =>
        Nil
    }
  }



  class RuleAnnotation(node: XHTMLNode) extends DeclarationAnnotation(node) with ToScript with HasTermArgs {
    override def getElement(implicit state: SemanticParsingState): List[RuleConstant] = _parent match {
      case Some(pt : TheoryAnnotation) =>
        List(rci(pt.path,OMAorAny(OMID(Path.parseM(resource)),getArgs.map(state.applyTopLevelTerm)),true))
    }
  }

  class TermConstantAnnotation(node: XHTMLNode) extends DeclarationAnnotation(node) with HasTermArgs {
    override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = _parent match {
      case Some(p : ModuleAnnotation) =>
        _terms match {
          case List(tm) =>
            Constant(OMMOD(p.path),p.newName(resource),Nil,None,Some(state.applyTopLevelTerm(tm)),Some(resource)) :: Nil
          case _ =>
            ???
        }
      case _ =>
        ???
    }
  }

  class VariableAnnotation(node: XHTMLNode) extends DeclarationAnnotation(node) with HasTermArgs {
    override def getElement(implicit state: SemanticParsingState): List[Constant] = _parent match {
      case Some(p : ModuleAnnotation) =>
        _terms match {
          case List(vd) =>
            var constants : List[String] = Nil
            val cs = context2list(state.makeBinder(vd)).map {vd =>
              val ret = Constant(OMMOD(p.path),p.newName(resource),Nil,vd.tp,vd.df,Some("variable"))
              ret.metadata.update(STeX.meta_vardecl,STeX.StringLiterals(resource))
              constants ::= ret.path.toString
              val orig = vd.metadata.getValues(STeX.meta_notation).head.asInstanceOf[Term]
              state.addTransform({case `orig` =>
                val omv = OMV(LocalName(resource))
                omv.metadata.update(STeX.meta_source,ret.toTerm)
                omv
              })
              ret
            }
            node.attributes(("","stex:constant")) = constants.mkString(" ")
            cs
          case _ =>
            ???
        }
      case _ =>
        ???
    }
  }

   */

  import DocumentExtension._

  override lazy val documentRules = List(
    {case thm: HTMLTheory =>
      sidebar(thm, (<b style="font-size: larger">Theory: {thm.name.toString}</b>) :: Nil)
    },
 /*   {case v: XHTMLVarDecl =>
      val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
      val seq = scala.xml.Text("Variable ") :: server.xhtmlPresenter.asXML(v.vardecl, None) :: is
      sidebar(v, seq)
    }, */
    {case s: HTMLSymbol =>
      controller.getO(s.path) match {
        case Some(c : Constant) =>
          sidebar(s,{<span>Constant {makeButton("/:" + server.pathPrefix + "/declaration?" + c.path,scala.xml.Text(c.name.toString)
          )}<code>(\\{s.macroname})</code></span>} :: Nil)
        case _ =>

      }
    },
    {case s: HTMLImport =>
      sidebar(s,{<span>Include {makeButton("/:" + server.pathPrefix + "/theory?" + s.domain,scala.xml.Text(s.name.toString))}</span>} :: Nil)
    },
    {case t: HasHeadSymbol => overlay(t,"/:" + server.pathPrefix + "/fragment?" + t.head.toString,"/:" + server.pathPrefix + "/declaration?" + t.head.toString)},
//    {case v : XHTMLOMV => overlay(v,"/:" + server.pathPrefix + "/fragment?" + v.path,"/:" + server.pathPrefix + "/declaration?" + v.path)},
/*    {case c : TermAnnotation if c.node.attributes.contains(("stex","constant")) => /* c.toTerm*/ /*{
      sidebar(c, {
        <span>{"Term:"}
          {DocumentExtension.makeButton("/:" + server.pathPrefix + "/declaration?" + c.attributes(("stex", "constant")),
            server.xhtmlPresenter.asXML(c.toTerm, None)).node}
        </span>
      }:: Nil)
    }*/}, */
/*    {case c : TermAnnotation if c.isTop && !c.toTerm.isInstanceOf[OMID] =>
      c.toTerm match {
        case OMS(_) =>
        case _ =>
      }
    } */
  )

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
              ???
          }
        case _ =>
      }
    case _ =>
  }
}