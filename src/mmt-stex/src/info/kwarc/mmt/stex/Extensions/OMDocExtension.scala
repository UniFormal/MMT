package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMA, OMAorAny, OMID}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, RuleConstant, RuleConstantInterpreter}
import info.kwarc.mmt.api.uom.AbbrevRule
import info.kwarc.mmt.api.{MPath, Path, StructuralElement}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.xhtml.{ArgumentAnnotation, ArityComponentA, ComponentAnnotation, ConstantAnnotation, DeclarationAnnotation, DefComponentA, HasHeadSymbol, HasTermArgs, MacronameComponentA, NotationComponentA, NotationFragmentComponentA, OMAAnnotation, OMBINDAnnotation, OMIDAnnotation, Plain, PreElement, PreParent, PreTheory, PrecedenceComponentA, SemanticParsingState, SourceRefAnnotation, StructureAnnotation, TermAnnotation, TheoryAnnotation, ToScript, TypeComponentA, XHTMLAnnotation, XHTMLDocument, XHTMLNode, XHTMLOMDoc}

object OMDocExtension extends DocumentExtension {

  override val checkingRules = List(
    {case (c : Constant,s) if c.rl.contains("notation") =>
      controller add c
      c
    }
  )

  override def start(args: List[String]): Unit = {
    super.start(args)
    if (!server.extensions.contains(NotationExtractor))
      controller.extman.addExtension(NotationExtractor)

  }

  class MetatheoryComponent(node : XHTMLNode) extends ComponentAnnotation(node) {
    var _metatheory : String = ""
    def metatheory : Option[MPath] = if (_metatheory == "") None else Some(Path.parseM(_metatheory))

    override def close(state: SemanticParsingState): Unit = {
      super.close(state)
      _parent match {
        case Some(t : TheoryAnnotation) =>
          t.addMeta(metatheory)
        case _ =>
          print("")
          ???
      }
    }
  }

  trait HasLanguage extends XHTMLAnnotation {
    var language = ""
  }

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

    override def getElement(implicit state: SemanticParsingState): List[Declaration] = {
      val ls = super.getElement
      if (_definientia.nonEmpty) {
          ls.head.asInstanceOf[FinalConstant].df.map {df =>
            ls ::: RuleConstant(ls.head.home,_parent.get.newName("abbrevrule"),OMA(OMID(Path.parseM("scala://features.stex.mmt.kwarc.info?AbbreviationRule")),
              List(ls.head.toTerm,df)),Some(new AbbrevRule(ls.head.path,df))) :: Nil
          }.getOrElse(ls)
      } else ls
    }
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

  lazy val rci = new RuleConstantInterpreter(controller)

  class RuleAnnotation(node: XHTMLNode) extends DeclarationAnnotation(node) with ToScript with HasTermArgs {
    override def getElement(implicit state: SemanticParsingState): List[RuleConstant] = _parent match {
      case Some(pt : TheoryAnnotation) =>
        List(rci(pt.path,OMAorAny(OMID(Path.parseM(resource)),getArgs),true))
    }
  }

  override lazy val xhtmlRules = List(
    {case (n,s) if n.attributes.get(("","property")).contains("srcref") => new SourceRefAnnotation(n) case _ =>},
    {case (n,s) if n.attributes.contains(("stex","srcref")) => new SourceRefAnnotation(n) case _ =>},
    XHTMLOMDoc.toRule("stex:args")((n,s) => new ArityComponentA(n)),
    XHTMLOMDoc.toRule("stex:type")((n,s) => new TypeComponentA(n)),
    XHTMLOMDoc.toRule("stex:definiens")((n,s) => new DefComponentA(n)),
    XHTMLOMDoc.toRule("stex:notationfragment")((n,s) => new NotationFragmentComponentA(n)),
    XHTMLOMDoc.toRule("stex:precedence")((n,s) => new PrecedenceComponentA(n)),
    XHTMLOMDoc.toRule("stex:notationcomp")((n,s) => new NotationComponentA(n)),
    XHTMLOMDoc.toRule("stex:macroname")((n,s) => new MacronameComponentA(n)),
    XHTMLOMDoc.toRule("stex:theory")((n,s) => new TheoryAnnotation(n)),
    XHTMLOMDoc.toRule("stex:symdecl")((n,s) => new SymdeclAnnotation(n)),
    XHTMLOMDoc.toRule("stex:notation")((n,s) => new NotationAnnotation(n)),
    XHTMLOMDoc.toRule("stex:metatheory")((n,s) => new MetatheoryComponent(n)),
    XHTMLOMDoc.toRule("stex:language")((n,s) => new LanguageComponent(n)),
    XHTMLOMDoc.toRule("stex:signature")((n,s) => new SignatureComponent(n)),
    XHTMLOMDoc.toRule("stex:import")((n,s) => new ImportAnnotation(n)),
    XHTMLOMDoc.toRule("stex:usemodule")((n,s) => new UseModuleAnnotation(n)),
    XHTMLOMDoc.toRule("stex:OMA")((n,s) => new OMAAnnotation(n)),
    XHTMLOMDoc.toRule("stex:OMID")((n,s) => new OMIDAnnotation(n)),
    XHTMLOMDoc.toRule("stex:OMBIND")((n,s) => new OMBINDAnnotation(n)),
    XHTMLOMDoc.toRule("stex:mmtrule")((n,s) => new RuleAnnotation(n)),
    {case (n,s) if n.attributes.get(("","property")).contains("stex:arg") => new ArgumentAnnotation(n) case _ =>},
    {case (n,s) if n.classes.exists(_.startsWith("stex:arg")) => new ArgumentAnnotation(n) case _ =>},
    {case (n,s) if n.attributes.contains(("stex","arg")) => new ArgumentAnnotation(n) case _ =>},
  )
  import DocumentExtension._

  override lazy val documentRules = List(
    {case thm: TheoryAnnotation =>
      sidebar(thm.node, (<b style="font-size: larger">Theory: {thm.name.toString}</b>) :: Nil)
    },
 /*   {case v: XHTMLVarDecl =>
      val is = List(if ((v.universal contains true) || v.universal.isEmpty) scala.xml.Text(" (universal)") else scala.xml.Text(" (existential)"))
      val seq = scala.xml.Text("Variable ") :: server.xhtmlPresenter.asXML(v.vardecl, None) :: is
      sidebar(v, seq)
    }, */
    {case s: SymdeclAnnotation =>
      controller.getO(s.path) match {
        case Some(c : Constant) =>
          sidebar(s.node,{<span>Constant {makeButton("/:" + server.pathPrefix + "/declaration?" + c.path,scala.xml.Text(c.name.toString)
          ).node} <code>{PreElement.getMacroName(c).map(s => "(\\" + s + ")").getOrElse("")}</code></span>} :: Nil)
        case _ =>

      }
    },
    {case s: ImportAnnotation =>
      sidebar(s.node,{<span>Include {makeButton("/:" + server.pathPrefix + "/theory?" + s.resource,scala.xml.Text(s.name.toString)).node}</span>} :: Nil)
    },
    {case t: HasHeadSymbol => overlay(t.node,"/:" + server.pathPrefix + "/fragment?" + t.head.toString,"/:" + server.pathPrefix + "/declaration?" + t.head.toString)},
//    {case v : XHTMLOMV => overlay(v,"/:" + server.pathPrefix + "/fragment?" + v.path,"/:" + server.pathPrefix + "/declaration?" + v.path)},
    {case c : TermAnnotation if c.node.attributes.contains(("stex","constant")) => /* c.toTerm*/ /*{
      sidebar(c, {
        <span>{"Term:"}
          {DocumentExtension.makeButton("/:" + server.pathPrefix + "/declaration?" + c.attributes(("stex", "constant")),
            server.xhtmlPresenter.asXML(c.toTerm, None)).node}
        </span>
      }:: Nil)
    }*/},
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
            case _ =>
              ???
          }
        case _ =>
      }
    case _ =>
  }
}