package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{DRef, Document, SectionLevel}
import info.kwarc.mmt.api.{DPath, GeneratedDRef, GlobalName, LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMFOREIGN, OML, OMMOD, OMS, OMV, Term}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, Include}
import info.kwarc.mmt.odk.OpenMath.OMForeign
import info.kwarc.mmt.stex.Extensions.{ImportStep, LateBinding, SectionStep, SlideStep, StatementStep}
import info.kwarc.mmt.stex.{SHTML, SHTMLHoas}

import scala.util.Try


abstract class SHTMLNode(orig: HTMLNode,val key : Option[String] = None) extends HTMLNodeWrapper(orig) with SHTMLObject {
  type SHTMLClass = SHTMLNode
  private[stex] var removed: List[String] = orig match {
    case sn: SHTMLNode => key.toList ::: sn.removed
    case _ => key.toList
  }
  def sstate = state match {
    case ss:SHTMLState[SHTMLNode] => Some(ss)
    case _ => None
  }

  override def findAncestor[A](f: PartialFunction[SHTMLNode, A]): Option[A] = {
    this.collectAncestor{
      case o : SHTMLNode if matches(o,f) => applyF(o,f)
    }
  }
  private def matches[A](n : HTMLNode,f: PartialFunction[SHTMLNode, A]) : Boolean = n match {
    case o : SHTMLNode if f.isDefinedAt(o) => true
    case o : SHTMLNode => matches(o.inner,f)
    case _ => false
  }
  private def applyF[A](n : SHTMLNode,f : PartialFunction[SHTMLNode, A]) : A = n match {
    case o if f.isDefinedAt(o) => f(o)
    case o => applyF(o.inner.asInstanceOf[SHTMLNode],f)
  }

  private var old_in_term : Boolean = sstate.exists(_.in_term)
  protected def set_in_term = sstate.foreach(_.in_term = true)
  protected def reset_in_term = sstate.foreach(_.in_term = old_in_term)

  def doSourceRef(hm : HasMetaData) =
    sourceref.foreach(sr => SourceRef.update(hm,sr))
  override def onAdd: Unit = {
    orig match {
      case sn: SHTMLNode => sn.onAdd
      case _ =>
        def filter(n: HTMLNode): Boolean = n match {
          case t: HTMLText if t.text == "&#8205;" => false
          case t: HTMLText if t.text == "&#160;" => false
          case n: HTMLPlainNode if n.label == "mrow" => n._children.exists {
            case t: HTMLText if t.text == "&#8205;" => false
            case _ => true
          }
          case n: SHTMLVisible if n.plain.attributes.get((HTMLParser.ns_shtml, "visible")).contains("false") && n.children.isEmpty => false
          case n: SHTMLVisible if n.plain.attributes.get((HTMLParser.ns_shtml, "visible")).contains("false") => n.children.exists {
            case t: HTMLText if t.text == "&#8205;" || t.text == "&nbsp;" => false
            case _ => true
          }
          case _ => true
        }

        plain._children = plain._children.filter(n => filter(n))
    }
  }

  def getTerm: Term = {
    getTermI match {
      case Some(tm) => return sstate.map(_.applyTerm(tm)).getOrElse(tm)
      case _ =>
    }
    orig match {
      case sn : SHTMLNode => sn.getTermI match {
        case Some(tm) => return sstate.map(_.applyTerm(tm)).getOrElse(tm)
        case _ =>
      }
      case _ =>
    }
    children match {
      case List(a : SHTMLNode) => a.getTerm
      case List(a) if !a.plain.isInstanceOf[HTMLText] =>
        sstate.map(_.applyTerm(getInformal(a))).getOrElse(getInformal(a))
      case _ => sstate.map(_.applyTerm(getInformal(this))).getOrElse(getInformal(this))
    }
  }

  private def getInformal(n : HTMLNode) : Term = {
    val ret = n.children match {
      case List(_ : HTMLText) | Nil =>
        val pl = n.plain.plaincopy
        pl.attributes.keys.filter(_._1 == HTMLParser.ns_shtml).foreach(pl.attributes.remove)
        SHTML.informal(pl.node.head)
      case _ =>
        val args = n.children.flatMap {
          case o: IsTerm => Some(o.getTerm)
          case o => Some(getInformal(o))
        }
        SHTML.informal.op(n.label, args)
    }
    doSourceRef(ret)
    ret
  }
  protected def getTermI : Option[Term] = None
}

abstract class SHTMLRule(override val priority: Int = 0) extends HTMLRule {
  def removed(n : HTMLNode) : List[String] = n match {
    case sn : SHTMLNode => sn.removed ::: removed(sn.inner)
    case _ => Nil
  }
  def apply(s: HTMLParser.ParsingState, n: HTMLNode): Option[HTMLNodeWrapper] = {
    val pairs = n.plain.attributes.collect { case s if s._1._1 == HTMLParser.ns_shtml => (s._1._2, s._2) }.toList
      .filterNot(p => removed(n).contains(p._1))
    //if (pairs.nonEmpty) {
      apply(s, n, pairs)
    //} else None
  }

  def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode]
}
case class SHTMLParsingRule(key:String, newobj: (String,HTMLNode,Option[SHTMLState[SHTMLNode]]) => SHTMLNode, override val priority:Int = 0) extends SHTMLRule(priority) {
  def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
    attrs.find(_._1 == key).map {p =>
      try {
        newobj(p._2,n,s match {case ss : SHTMLState[SHTMLNode] => Some(ss) case _ => None})
      } catch {
        case _ => return None
      }
    }
  }
}

case class SHTMLDocument(path : DPath, orig:HTMLNode) extends SHTMLNode(orig) with SHTMLODocument {
  override def copy = SHTMLDocument(path,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { state =>
      doc.foreach{d =>
        val tm = state.bindings.term
        //al num = state.bindings.toNum(state.server.ctrl)
        d.metadata.update(LateBinding.sym,tm)
      }
    }
  }
}

case class SHTMLVisible(visible:Boolean,orig:HTMLNode) extends SHTMLNode(orig,Some("visible")) with SHTMLOVisible {
  override def copy = SHTMLVisible(visible,orig.copy)
}

class SHTMLTheory(val mp : MPath,orig:HTMLNode) extends SHTMLNode(orig,Some("theory"))
  with SHTMLOTheory {
  override def copy = {
    val ret = new SHTMLTheory(mp,orig.copy)
    ret.metatheory = metatheory
    ret.signature = signature
    ret.language = language
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    ret.variables = variables
    ret._context = _context
    ret
  }

  plain.attributes.get((HTMLParser.ns_shtml, "metatheory")).foreach(s => Try(this.metatheory = Some(Path.parseM(s))))
  removed ::= "metatheory"
  plain.attributes.get((HTMLParser.ns_shtml, "signature")).foreach(s => signature = s )
  removed ::= "signature"
  plain.attributes.get((HTMLParser.ns_shtml, "language")).foreach(s => language = s)
  removed ::= "language"
  removed ::= "problem"

  open

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}

case class SHTMLDocumentTitle(orig:HTMLNode) extends SHTMLNode(orig,Some("doctitle")) {
  override def copy = SHTMLDocumentTitle(orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.addTitle(this))
  }
}

case class SHTMLImportModule(mp:MPath,orig:HTMLNode) extends SHTMLNode(orig,Some("import"))
  with SHTMLOImportModule {
  override def copy: HTMLNode = SHTMLImportModule(mp,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}

case class SHTMLUseModule(mp:MPath,orig:HTMLNode) extends SHTMLNode(orig,Some("usemodule"))
  with SHTMLOUseModule {
  override def copy: HTMLNode = SHTMLImportModule(mp,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}

trait SHTMLSymbolLike extends SHTMLNode with SymbolLike {

  protected def copyI[A <: SHTMLSymbolLike](newst: A):A = {
    newst.macroname = macroname
    newst.assoctype = assoctype
    newst.reorderargs = reorderargs
    newst.args = args
    newst.roles = roles
    newst
  }

  plain.attributes.get((HTMLParser.ns_shtml, "args")).foreach(s => args = s)
  removed ::= "args"
  removed ::= "macroname"
  macroname = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "macroname"), "")
  removed ::= "assoctype"
  assoctype = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "assoctype"), "")
  removed ::= "reorderargs"
  reorderargs = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "reorderargs"), "")
  removed ::= "role"
  roles = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "role"), "").split(',').map(_.trim).toList
}

case class SHTMLArgTypes(orig:HTMLNode) extends SHTMLNode(orig,Some("argtypes")) {
  def copy = SHTMLArgTypes(orig.copy)
}

class SHTMLSymbol(val path:GlobalName, orig:HTMLNode) extends SHTMLNode(orig,Some("symdecl"))
  with SHTMLSymbolLike with SHTMLOSymbol {

  override def copy: HTMLNode = {
    val ret = new SHTMLSymbol(path,orig.copy)
    copyI(ret)
    ret.types = types
    ret.defi = defi
    ret.return_type = return_type
    ret
  }

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}
class SHTMLVardef(val name:LocalName, orig:HTMLNode) extends SHTMLNode(orig,Some("vardef"))
  with SHTMLSymbolLike with SHTMLOVarDecl {

  removed ::= "bind"
  this.plain.attributes.get((HTMLParser.ns_shtml, "bind")) match {
    case Some(_) => bind = true
    case _ =>
  }

  override def copy: HTMLNode = {
    val ret = new SHTMLVardef(name,orig.copy)
    copyI(ret)
    ret.types = types
    ret.defi = defi
    ret.return_type = return_type
    ret.bind = bind
    ret
  }

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}

case class SHTMLBind(name:LocalName, orig:HTMLNode) extends SHTMLNode(orig,Some("bind")) {
  override def copy: HTMLNode = SHTMLBind(name,orig.copy)

  override def onAdd(): Unit = sstate.foreach { state =>
    findAncestor {
      case gl: SHTMLGroupLike => gl
    }.foreach {gl =>
      val vars = gl.getVariables
      vars.findLast(_.name == name).foreach{ vd =>
        val nvd = state.markAsBound(vd.copy())
        gl.variables ++= nvd
      }
    }
  }
}

class SHTMLVarseq(val name:LocalName, orig:HTMLNode) extends SHTMLNode(orig,Some("varseq"))
  with SHTMLSymbolLike with SHTMLOVarDecl {

  override def copy: HTMLNode = {
    val ret = new SHTMLVardef(name,orig.copy)
    copyI(ret)
    ret.types = types
    ret.defi = defi
    ret.return_type = return_type
    ret
  }

  override def onAdd: Unit = {
    super.onAdd
    this.closeSeq
  }
}

case class SHTMLType(orig:HTMLNode) extends SHTMLNode(orig,Some("type")) {
  set_in_term
  override def copy: HTMLNode = SHTMLType(orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    val tm = getTerm
    collectAncestor{case ht : HasTypes => ht}.foreach{ht =>
      ht.types ::= tm
    }
  }
}
case class SHTMLDefiniens(orig:HTMLNode) extends SHTMLNode(orig,Some("definiens")) {
  set_in_term
  override def copy: HTMLNode = SHTMLDefiniens(orig.copy)
  lazy val path = this.plain.attributes.get((HTMLParser.ns_shtml, "definiens")) match {
    case Some(s) if s.nonEmpty => Some(Path.parseS(s))
    case _ => None
  }

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    val tm = getTerm
    path match {
      case Some(p) =>
        findAncestor { case mod : ModuleLike => mod}.foreach { _.signature_theory match {
          case Some(th) if th.path == p.module =>
            sstate.foreach { state =>
              state.getO(p) match {
                case Some(c : Constant) =>
                  val (df,tp) = matchterms(Some(state.applyTopLevelTerm(tm)),c.tp)
                  val nc = Constant(c.home,c.name,c.alias,tp,df,c.rl)
                  nc.metadata = c.metadata
                  state.update(nc)
                  state.check(nc)
                case _ =>
              }
            }
          case _ =>
            print("")
        }}
      case None =>
        findAncestor { case ht: HasDefiniens => ht }.foreach { ht =>
          ht.defi = Some(tm)
        }
    }
  }
}


case class SHTMLConclusion(orig:HTMLNode) extends SHTMLNode(orig,Some("conclusion")) {
  set_in_term
  override def copy: HTMLNode = SHTMLDefiniens(orig.copy)
  lazy val path = this.plain.attributes.get((HTMLParser.ns_shtml, "conclusion")) match {
    case Some(s) if s.nonEmpty => Some(Path.parseS(s))
    case _ => None
  }

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    path match {
      case Some(p) =>
        findAncestor { case mod : ModuleLike => mod}.foreach { _.signature_theory match {
          case Some(th) if th.path == p.module =>
            sstate.foreach { state =>
              state.getO(p) match {
                case Some(c : Constant) =>
                  var judg: Option[(Term,Option[SHTMLHoas.HoasRule])] = None
                  try {
                    self.getRuleContext.getIncludes.foreach { i =>
                      state.server.ctrl.globalLookup.forDeclarationsInScope(OMMOD(i)) {
                        case (_, _, c: Constant) if c.rl.map(r => r.split(' ').toList).getOrElse(Nil).contains("judgment") =>
                          state.getRuler(c.toTerm) match {
                            case Some(r) =>
                              judg = Some((c.toTerm, SHTMLHoas.get(r)))
                            case _ => judg = Some((c.toTerm, None))
                          }
                        case _ =>
                      }
                    }
                  } catch {
                    case e : info.kwarc.mmt.api.Error => state.error(e)
                  }
                  def doTerm(tm : Term) : Term = tm match {
                    case OMBIND(OMS(ParseResult.unknown),ctx,bd) =>
                      OMBIND(OMS(ParseResult.unknown),ctx,doTerm(bd))
                    case SHTML.implicit_binder.spine(ctx,bd) =>
                      SHTML.implicit_binder(ctx,doTerm(bd))
                    case t => judg match {
                      case Some((tm, Some(h))) => h.HOMA(tm, List(t))
                      case Some((tm, None)) => OMA(tm, List(t))
                      case _ => t
                    }
                  }
                  val tp = doTerm(state.applyTopLevelTerm(getTerm))
                  val nc = Constant(c.home,c.name,c.alias,Some(tp),None,c.rl)
                  nc.metadata = c.metadata
                  state.update(nc)
                  state.check(nc)
                case _ =>
              }
            }
          case _ =>
            print("")
        }}
      case None =>
    }
  }
}

case class SHTMLReturnType(orig:HTMLNode) extends SHTMLNode(orig,Some("returntype")) {
  set_in_term
  override def copy = SHTMLReturnType(orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    val tm = getTerm
    collectAncestor{case sl:SymbolLike => sl}.foreach{ sl =>
      sl.return_type = Some(tm)
    }
  }
}
trait HTMLIsTerm extends SHTMLNode with IsTerm {
  val notationid = plain.attributes.find(p => p._1._1 == HTMLParser.ns_shtml && p._1._2 == "notationid").map(_._2).getOrElse("")
  removed ::= "notationid"
  removed ::= "head"

  lazy val headsymbol: Option[Term] = plain.attributes.get((HTMLParser.ns_shtml,"head")).flatMap { s =>
    if (s.split('?').length > 2) {
      Try({
        val gn = Path.parseS(s)
        val t = OMS(gn)
        doSourceRef(t)
        t
      }).toOption
    }
    else if (s.contains('?')) {
      Try({
        val gn = Path.parseM(s)
        val t = OMMOD(gn)
        doSourceRef(t)
        t
      }).toOption
    } else {
      val t = OMV(s)
      doSourceRef(t)
      Some(t)
    }
  }

  //set_in_term
  override def onAdd: Unit = {
    super.onAdd
    //reset_in_term
  }
}

case class SHTMLArg(ind:Int,orig:HTMLNode) extends SHTMLNode(orig,Some("arg"))
  with SHTMLOArg {
  removed ::= "argmode"
  lazy val argmode = plain.attributes((HTMLParser.ns_shtml,"argmode"))
  override def copy: HTMLNode = SHTMLArg(ind,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
}

case class SHTMLHeadTerm(orig:HTMLNode) extends SHTMLNode(orig,Some("headterm")) {
  override def copy: HTMLNode = SHTMLHeadTerm(orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    findAncestor{case ht:HasTerm => ht}.foreach{ht =>
      ht.term = Some(getTerm)
    }
  }
}

case class TopLevelTerm(orig:HTMLNode) extends SHTMLNode(orig) with SHTMLOTopLevelTerm {
  override def copy: HTMLNode = TopLevelTerm(orig)
  set_in_term

  override def onAdd: Unit = {
    reset_in_term
    super.onAdd
    close.foreach {c => plain.attributes((HTMLParser.ns_mmt,"constant")) = c.path.toString}
  }
}

case class SHTMLOMB(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOOMB with HTMLIsTerm {
  override def copy: HTMLNode = SHTMLOMB(orig.copy)
}

case class SHTMLOMA(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOOMA with HTMLIsTerm {
  override def copy: HTMLNode = SHTMLOMA(orig.copy)
}

case class SHTMLOMS(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOMIDorOMV with HTMLIsTerm {
  override def copy: HTMLNode = SHTMLOMS(orig.copy)
}

case class SHTMLOML(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOMIDorOMV with HTMLIsTerm with HasTypes with HasDefiniens {
  lazy val name = plain.attributes.get((HTMLParser.ns_shtml,"head")).map { s =>
    LocalName.parse(s)
  }
  override def copy: HTMLNode = {
    val ret = SHTMLOML(orig.copy)
    ret.types = types
    ret.defi = defi
    ret
  }

  override def getTermI: Option[Term] = {
    name.map{n =>
      OML(n,types.headOption,defi,None,None)
    }
  }
}

case class SHTMLOMMOD(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOMIDorOMV with HTMLIsTerm {
  override def copy: HTMLNode = SHTMLOMS(orig.copy)
}

case class SHTMLOMV(orig:HTMLNode) extends SHTMLNode(orig,Some("term"))
  with SHTMLOMIDorOMV with HTMLIsTerm {
  override def copy: HTMLNode = SHTMLOMV(orig.copy)
}

case class SHTMLComp(path:GlobalName,orig:HTMLNode) extends SHTMLNode(orig,Some("comp")) {
  override def copy: HTMLNode = SHTMLComp(path,orig.copy)
}
case class SHTMLVarComp(name:LocalName,orig:HTMLNode) extends SHTMLNode(orig,Some("varcomp")) {
  override def copy: HTMLNode = SHTMLVarComp(name,orig.copy)
}

case class SHTMLNotation(path:GlobalName,orig:HTMLNode) extends SHTMLNode(orig,Some("notation"))
with SHTMLONotation {
  override def copy: HTMLNode = SHTMLNotation(path,orig.copy)
  removed ::= "precedence"
  removed ::= "notationfragment"
  id = this.plain.attributes.getOrElse((HTMLParser.ns_shtml,"notationfragment"),"")
  opprec = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "precedence"), "0").toInt
  override def onAdd: Unit = {
    super.onAdd
    close(path)
  }
}

case class SHTMLVarNotation(name:LocalName,orig:HTMLNode) extends SHTMLNode(orig,Some("notation"))
  with SHTMLONotation {
  override def copy: HTMLNode = SHTMLVarNotation(name,orig.copy)
  removed ::= "precedence"
  removed ::= "notationfragment"
  id = this.plain.attributes.getOrElse((HTMLParser.ns_shtml,"notationfragment"),"")
  opprec = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "precedence"), "0").toInt
  override def onAdd: Unit = {
    super.onAdd
    closeVar(name)
  }
}

case class SHTMLNotationComponent(orig:HTMLNode) extends SHTMLNode(orig,Some("notationcomp"))
  with SHTMLONotationComponent {
  def copy = SHTMLNotationComponent(orig.copy)
  set_in_term
  override def onAdd: Unit = {
    reset_in_term
    super.onAdd
    close
  }
}
case class SHTMLOpNotationComponent(orig:HTMLNode) extends SHTMLNode(orig,Some("notationopcomp"))
  with SHTMLOOpNotationComponent {
  def copy = SHTMLOpNotationComponent(orig.copy)
  set_in_term
  override def onAdd: Unit = {
    reset_in_term
    super.onAdd
    close
  }
}
case class SHTMLArgnum(ind:Int,orig:HTMLNode) extends SHTMLNode(orig,Some("argnum")) {
  override def copy: HTMLNode = SHTMLArgnum(ind,orig.copy)
  removed ::= "precedence"
  val precedence : Int = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "precedence"), "0").toInt
}
class SHTMLMathStructure(val mp:MPath,orig:HTMLNode) extends SHTMLNode(orig,Some("feature-structure"))
  with SHTMLOMathStructure {

  removed ::= "macroname"
  val macroname = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "macroname"), mp.name.toString)
  override def copy: HTMLNode = {
    val ret = new SHTMLMathStructure(mp,orig.copy)
    ret.parent = parent
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    ret
  }

  override def onOpen: Unit = open
  override def onAdd: Unit = {
    super.onAdd
    close
  }
}
case class SHTMLMMTRule(override val path:MPath,orig:HTMLNode) extends SHTMLNode(orig,Some("rule"))
  with SHTMLORule {
  override def copy = SHTMLMMTRule(path,orig.copy)
  set_in_term
  override def onAdd: Unit = {
    reset_in_term
    super.onAdd
    close
  }
}

case class SHTMLInputref(target:String,orig : HTMLNode) extends SHTMLNode(orig,Some("inputref")) {
  override def copy: HTMLNode = new SHTMLInputref(target,orig.copy) {
    override def init = {}
  }
  def init = {
    sstate.foreach { state =>
      findAncestor { case t: SHTMLDocument => t }.foreach { doc =>
        val rtarget = Path.parseD((if (target.endsWith(".tex")) target.dropRight(4) else target) + ".omdoc",NamespaceMap.empty)
        val dref = DRef(doc.path, rtarget)
        dref.setOrigin(GeneratedDRef)
        doSourceRef(dref)
        state.add(dref)
        state.bindings.add(ImportStep(rtarget))
      }
    }
  }
  init
}

abstract class HTMLStatement(val kind:String,orig:HTMLNode) extends SHTMLNode(orig,Some(kind))
  with SHTMLStatement with SHTMLSymbolLike {

  removed ::= "styles"
  var styles = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "styles"), "").split(',').map(_.trim).toList
  removed ::= "fors"
  fors = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "fors"), "").split(',').flatMap(
    s => Try(Path.parseS(s.trim)).toOption
  ).toList
  removed ::= "id"
  id = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "id"), "")

  var title : Option[SHTMLTitle] = None

  protected def copyII[A <: HTMLStatement](newst: A):A = {
    copyI(newst)
    newst.fors = fors
    newst.styles = styles
    newst.id = id
    newst.title = title.map(_.copy)
    newst
  }

  override def onAdd: Unit = {
    sstate.foreach { state =>
      state.bindings.add(StatementStep)
    }
    super.onAdd
  }
}

case class SHTMLTitle(orig:HTMLNode) extends SHTMLNode(orig,Some("statementtitle")) {
  override def onAdd: Unit = {
    super.onAdd
    findAncestor{
      case st:HTMLStatement => st
    }.foreach(_.title = Some(this))
  }

  override def copy = SHTMLTitle(orig.copy)
}

case class SHTMLDefiniendum(path:GlobalName,orig:HTMLNode) extends SHTMLNode(orig,Some("definiendum")) {
  override def copy: HTMLNode = SHTMLDefiniendum(path,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    findAncestor{case st:HTMLStatement => st}.foreach(s => s.fors = (path :: s.fors).distinct)
  }
}
case class SHTMLParagraph(orig:HTMLNode) extends HTMLStatement("paragraph",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLParagraph(orig))
  }

  override def onAdd: Unit = {
    super.onAdd
    if (styles.contains("symdoc")) {
      sstate.foreach(_.addSymdoc(fors,id,this.plain.node.head))
    }
  }
}
case class SHTMLExample(orig:HTMLNode) extends HTMLStatement("example",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLExample(orig))
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.addExample(fors, id, this.plain.node.head))
  }
}
case class SHTMLDefinition(orig:HTMLNode) extends HTMLStatement("definition",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLDefinition(orig))
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.addSymdoc(fors,id,this.plain.node.head))
  }
}
case class SHTMLAssertion(orig:HTMLNode) extends HTMLStatement("assertion",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLAssertion(orig))
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.addSymdoc(fors, id, this.plain.node.head))
  }
}

case class SHTMLMMTStructure(path:GlobalName,orig:HTMLNode) extends SHTMLNode(orig,Some("feature-morphism"))
with SHTMLMorphism {
  removed :::= List("total","domain")
  val domain = Path.parseM(this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "domain"), "").trim)
  val istotal = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "total"),"").trim == "true"

  def copy = {
    val ret = SHTMLMMTStructure(path,orig.copy)
    ret.structure = structure
    ret
  }

  def getAss(gn:GlobalName) : Constant = {
    val name = LocalName(gn.module) / gn.name
    structure.get.getO(name) match {
      case Some(c : Constant) =>
        c
      case _ =>
        val c = Constant(structure.get.toTerm,name,Nil,None,None,None)
        /*sstate.get.getO(gn) match {
          case Some(oldc : Constant) =>
          case _ =>
        }*/
        c.metadata.update(SHTML.headterm,OMS(gn))
        sstate.get.add(c)
        c
    }
  }

  override def onOpen: Unit = {
    super.onOpen
    open()
  }

  override def onAdd: Unit = {
    super.onAdd
    close()
  }
}

case class SHTMLRenaming(path:GlobalName,orig:HTMLNode) extends SHTMLNode(orig,Some("rename")) {
  def copy = SHTMLRenaming(path, orig.copy)

  removed ::= "macroname"
  val macroname = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "macroname"), "")

  removed ::= "to"
  val to = this.plain.attributes.get((HTMLParser.ns_shtml, "to")).map(LocalName.parse)

  sstate.foreach { state =>
    findAncestor { case s: SHTMLMMTStructure => s }.foreach { structure =>
      val orig = structure.getAss(path)
      if (macroname != "") {
        state.server.addMacroName(macroname, orig)
      }
      to match {
        case Some(name) =>
          val nc = Constant(orig.home, orig.name, name :: orig.alias, orig.tp, orig.df, orig.rl)
          nc.metadata = orig.metadata
          state.update(nc)
        case _ =>
      }
    }
  }
}

case class SHTMLAssignment(path: GlobalName, orig: HTMLNode) extends SHTMLNode(orig, Some("assign"))
  with HasDefiniens {
  def copy = SHTMLAssignment(path, orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { state =>
      findAncestor { case s: SHTMLMMTStructure => s }.foreach { structure =>
        defi.foreach { defi =>
          val orig = structure.getAss(path)
          val nc = Constant(orig.home, orig.name, orig.alias, orig.tp, Some(state.applyTopLevelTerm(defi)), orig.rl)
          nc.metadata = orig.metadata
          state.update(nc)
          state.check(nc)
        }
      }
    }
  }
}

class SHTMLFrame(orig : HTMLNode) extends SHTMLNode(orig, Some("frame")) {
  //this.plain.classes ::= "frame"
  def init = sstate.foreach{state =>
    state.bindings.add(SlideStep)
  }

  override def copy: HTMLNode = new SHTMLFrame(orig.copy) {
    override def init = {}
  }


  init
}
case class SHTMLFrameNumber(orig:HTMLNode) extends SHTMLNode(orig,Some("framenumber")) {
  def copy = SHTMLFrameNumber(orig.copy)
}

case class SHTMLSection(orig: HTMLNode) extends SHTMLNode(orig,Some("section")) {
  val lvl = this.plain.attributes.get((HTMLParser.ns_shtml, "section")).map(_.trim.toInt)
  def init = sstate.foreach{state =>
    state.bindings.add(new SectionStep(lvl.getOrElse(-1)))
    state match {
      case s : SemanticState =>
        val nd = new Document(s.doc.path / this.hashCode().toHexString, SectionLevel)
        s.add(nd)
        s.docs ::= nd
      case _ =>
    }
    state.doc
  }
  var title : Option[SHTMLSectionTitle] = None

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.bindings.close)
    sstate match {
      case Some(s : SemanticState) =>
        val d = s.docs.head
        title.foreach(t =>
          d.metadata.update(SHTML.mmtmeta_path ? "title",OMFOREIGN(t.plain.node))
        )
        s.docs = s.docs.tail
      case _ =>
    }
  }

  override def copy = {
    val ret = new SHTMLSection(orig.copy) { override def init = {}}
    ret.title=title.map(_.copy)
    ret
  }

  init
}
case class SHTMLSectionLevel(lvl:Int,orig:HTMLNode) extends SHTMLNode(orig,Some("sectionlevel")) {
  def copy = SHTMLSectionLevel(lvl,orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.bindings.topsec = lvl)
  }
}

case class SHTMLSectionTitle(orig: HTMLNode) extends SHTMLNode(orig,Some("sectiontitle")) {
  override def onAdd: Unit = {
    super.onAdd
    findAncestor{
      case sec: SHTMLSection => sec
    }.foreach(_.title = Some(this))
  }
  def copy= SHTMLSectionTitle(orig.copy)
}

trait ProofStep extends SHTMLNode {
  var yields: Option[Term] = None
}
trait ProofEnv extends SHTMLNode with ProofStep {
  val annotname : String = key.get
  removed ::= "proofhide"
  lazy val expanded = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "proofhide"), "") == "true"

  lazy val fors = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, annotname), "").split(',').flatMap(
    s => Try(Path.parseS(s.trim)).toOption
  ).toList
}
case class SHTMLProof(orig:HTMLNode) extends SHTMLNode(orig,Some("proof"))
  with ProofEnv {
  def copy = SHTMLProof(orig.copy)
}
case class SHTMLSubProof(orig:HTMLNode) extends SHTMLNode(orig,Some("subproof"))
  with ProofEnv {
  def copy = SHTMLSubProof(orig.copy)
}
case class SHTMLProofMethod(orig:HTMLNode) extends SHTMLNode(orig,Some("proofmethod")){
  def copy = SHTMLProofMethod(orig.copy)
}
case class SHTMLProofTerm(orig:HTMLNode) extends SHTMLNode(orig,Some("proofterm")) {
  set_in_term
  def copy = SHTMLProofTerm(orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    sstate.foreach{state =>
      findAncestor{case pe:ProofStep => pe}.foreach{ pe =>
        pe.yields = Some(getTerm)
      }
    }
  }
}
case class SHTMLProofStep(orig:HTMLNode) extends SHTMLNode(orig,Some("spfstep"))
  with ProofStep {
  def copy = SHTMLProofStep(orig.copy)
}
case class SHTMLProofConclusion(orig:HTMLNode) extends SHTMLNode(orig,Some("spfconclusion"))
  with ProofStep {
  def copy = SHTMLProofConclusion(orig.copy)
}
case class SHTMLProofEqStep(orig:HTMLNode) extends SHTMLNode(orig,Some("spfeqstep"))
  with ProofStep {
  def copy = SHTMLProofEqStep(orig.copy)
}
case class SHTMLProofAssumption(orig:HTMLNode) extends SHTMLNode(orig,Some("spfassumption"))
  with ProofStep {
  def copy = SHTMLProofAssumption(orig.copy)
}
case class SHTMLProofTitle(orig:HTMLNode) extends SHTMLNode(orig,Some("prooftitle")){
  def copy=SHTMLProofTitle(orig.copy)
}
case class SHTMLProofBody(orig:HTMLNode) extends SHTMLNode(orig,Some("proofbody")){
  def copy=SHTMLProofBody(orig.copy)
}
case class SHTMLFillInSol(orig:HTMLNode) extends SHTMLNode(orig,Some("fillinsol")){
  def copy=SHTMLFillInSol(orig:HTMLNode)
}
case class SHTMLMCB(orig:HTMLNode) extends SHTMLNode(orig,Some("multiple-choice-block")){
  def copy=SHTMLMCB(orig:HTMLNode)
}
case class SHTMLMCC(orig:HTMLNode) extends SHTMLNode(orig,Some("mcc")){
  def copy=SHTMLMCC(orig:HTMLNode)
}
case class SHTMLMCSol(orig:HTMLNode) extends SHTMLNode(orig,Some("mcc-solution")){
  def copy=SHTMLMCSol(orig:HTMLNode)
}
case class SHTMLSolution(orig: HTMLNode) extends SHTMLNode(orig,Some("solution")){
  def copy=SHTMLSolution(orig)
}