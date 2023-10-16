package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{DRef, Document, SectionLevel}
import info.kwarc.mmt.api.{DPath, GeneratedDRef, GlobalName, LocalName, MPath, NamespaceMap, Path, Rule, RuleSet}
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Obj.getConstants
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMFOREIGN, OML, OMMOD, OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{DatatypeProperty, RDFImplicits, ULO, ULOStatement}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, Include}
import info.kwarc.mmt.odk.OpenMath.OMForeign
import info.kwarc.mmt.stex.Extensions.{BlindSectionStep, ImportStep, LateBinding, SHTMLContentManagement, SectionStep, SlideStep, StatementStep}
import info.kwarc.mmt.stex.rules.{AssertionBinderRule, StringLiterals}
import info.kwarc.mmt.stex.{SHTML, SHTMLHoas}
import org.eclipse.rdf4j.model.{IRI, Resource, Value}

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
          case n: HTMLPlainNode if n.label == "mrow" && n.attributes.keys.forall(_._1 != HTMLParser.ns_shtml) => n._children.exists {
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

  protected def getRule[A <: Rule](cls:Class[A]) : Option[A] = {
    val state = sstate.get
    try {
      RuleSet.collectRules(state.server.ctrl, self.getRuleContext).getAll.collectFirst {
        case rl if rl.getClass == cls => rl.asInstanceOf[A]
      }
    } catch {
      case e: info.kwarc.mmt.api.Error =>
        state.error(e)
        None
    }
  }

  protected def getWithRole(role: String) = {
    val state = sstate.get
    try {
      var ret: Option[(Term, Option[SHTMLHoas.HoasRule])] = None
      self.getRuleContext.getIncludes.foreach { i =>
        state.server.ctrl.globalLookup.forDeclarationsInScope(OMMOD(i)) {
          case (_, _, c: Constant) if c.rl.map(r => r.split(' ').toList).getOrElse(Nil).contains(role) =>
            state.getRuler(c.toTerm) match {
              case Some(r) =>
                ret = Some((c.toTerm, SHTMLHoas.get(r)))
              case _ => ret = Some((c.toTerm, None))
            }
          case _ =>
        }
      }
      ret
    } catch {
      case e: info.kwarc.mmt.api.Error =>
        state.error(e)
        None
    }
  }
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
        case e =>
          e.printStackTrace()
          return None
      }
    }
  }
}

case class SHTMLDocument(path : DPath, orig:HTMLNode) extends SHTMLNode(orig) with SHTMLODocument {
  override def copy = SHTMLDocument(path,orig.copy)
  path.last.split('.').init.lastOption match {
    case Some(s@("en"|"de"|"ar"|"bg"|"ru"|"fi"|"ro"|"tr"|"fr")) /* TODO */ => language = s
    case _ =>
  }

  override def onOpen: Unit = {
    super.onOpen
    open
  }

  override def onAdd: Unit = {
    super.onAdd
    close
    sstate.foreach { state =>
      doc.foreach{d =>
        val tm = state.bindings.term
        //al num = state.bindings.toNum(state.server.ctrl)
        d.metadata.update(LateBinding.sym,tm)
        state.closeDoc
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

  override def onOpen: Unit = {
    super.onOpen
    open
  }

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

  override def onOpen: Unit = {
    super.onOpen
    open
  }

  override def onAdd: Unit = {
    super.onAdd
    this.close
  }
  print("")
}
class SHTMLVardef(val name:LocalName, orig:HTMLNode) extends SHTMLNode(orig,Some("vardef"))
  with SHTMLSymbolLike with SHTMLOVarDecl {

  removed ::= "bind"
  this.plain.attributes.get((HTMLParser.ns_shtml, "bind")) match {
    case Some(_) => bind = true
    case _ =>
  }

  override def onOpen: Unit = {
    super.onOpen
    open
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

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { state =>
      findAncestor {
        case gl: SHTMLGroupLike => gl
      }.foreach {gl =>
        val vars = gl.getVariables
        vars.findLast(_.name == name).foreach{ vd =>
          val nvd = state.markAsBound(VarDecl(vd.name,vd.feature,vd.tp,vd.df,vd.not))
          vd.metadata.getAll.foreach(nvd.metadata.update)
          gl.variables ++= nvd
        }
      }
    }
  }
}

class SHTMLVarseq(val name:LocalName, orig:HTMLNode) extends SHTMLNode(orig,Some("varseq"))
  with SHTMLSymbolLike with SHTMLOVarDecl {

  override def onOpen: Unit = {
    super.onOpen
    open
  }

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
case class SHTMLDefiniens(orig:HTMLNode) extends SHTMLNode(orig,Some("definiens")) with Definitional {
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
                  val vars = getVariableContext.filter(state.isBound).reverse.distinctBy(_.name).reverse
                  val idf = (lambda.tmhoas,vars) match {
                    case (Some(_),ctx) if ctx.nonEmpty =>
                      state.applyTopLevelTerm(vars.foldRight(tm)((vd, t) => lambda(vd, t)),false)
                    case _ =>
                      state.applyTopLevelTerm(tm)
                  }
                  val (df,tp) = matchterms(Some(idf),c.tp)
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

trait Definitional extends SHTMLNode {
  object lambda {
    lazy val tmhoas = getWithRole("lambda")

    def apply(vd: VarDecl, bd: Term) = tmhoas match {
      case Some((tm, Some(h))) => sstate.get.applyTerm(SHTMLHoas.bound(Some(h), tm, vd, bd))
      case Some((tm, None)) => sstate.get.applyTerm(OMBIND(tm, Context(vd), bd))
      case None =>
        ???
    }

    def unapply(t: Term) = tmhoas match {
      case Some((tm, Some(h))) => t match {
        case SHTMLHoas.bound(Some(`h`), `tm`, vd, bd) => Some((vd, bd))
        case _ => None
      }
      case Some((tm, None)) => t match {
        case SHTMLHoas.bound(None, `tm`, vd, bd) => Some((vd, bd))
        case _ => None
      }
      case None => None
    }
  }
}

trait Propositional extends SHTMLNode {

  object judgment {
    lazy val tmhoas = getRule(classOf[AssertionBinderRule.AssertionRule]) match {
      case Some(rl) =>
        rl.ded.map {ded =>
          val state = self.sstate.get
          state.getRuler(OMS(ded)) match {
            case Some(r) => (OMS(ded),SHTMLHoas.get(r))
            case _ => (OMS(ded),None)
          }
        }
      case None => getWithRole("judgment")
    }
    def apply(t : Term) = tmhoas match {
      case Some((tm, Some(h))) => h.HOMA(tm, List(t))
      case Some((tm, None)) => OMA(tm, List(t))
      case None => t
    }
    def unapply(t : Term) = tmhoas match {
      case Some((tm, Some(h))) => t match {
        case SHTMLHoas.OmaSpine(Some(`h`),`tm`,List(a)) => Some(a)
        case _ => None
      }
      case Some((tm, None)) => t match {
        case OMA(`tm`,List(a)) => Some(a)
        case _ => None
      }
      case None => None
    }
  }

  object forall {
    lazy val tmhoas = getRule(classOf[AssertionBinderRule.AssertionRule]) match {
      case Some(rl) =>
        val state = self.sstate.get
        Some(state.getRuler(OMS(rl.forall)) match {
          case Some(r) => (OMS(rl.forall), SHTMLHoas.get(r))
          case _ => (OMS(rl.forall), None)
        })
      case None => getWithRole("forall")
    }

    def apply(vd:VarDecl,bd: Term) = tmhoas match {
      case Some((tm, Some(h))) => sstate.get.applyTerm(SHTMLHoas.bound(Some(h),tm,vd,bd))
      case Some((tm, None)) => sstate.get.applyTerm(OMBIND(tm, Context(vd),bd))
      case None =>
        ???
    }

    def unapply(t: Term) = tmhoas match {
      case Some((tm, Some(h))) => t match {
        case SHTMLHoas.bound(Some(`h`), `tm`, vd, bd) => Some((vd,bd))
        case _ => None
      }
      case Some((tm, None)) => t match {
        case SHTMLHoas.bound(None, `tm`, vd, bd) => Some((vd,bd))
        case _ => None
      }
      case None => None
    }
  }

  object implies {
    lazy val tmhoas = getRule(classOf[AssertionBinderRule.AssertionRule]) match {
      case Some(rl) =>
        val state = self.sstate.get
        Some(state.getRuler(OMS(rl.imply)) match {
          case Some(r) => (OMS(rl.imply), SHTMLHoas.get(r))
          case _ => (OMS(rl.imply), None)
        })
      case None => getWithRole("implication")
    }

    def apply(t1: Term,t2:Term) = tmhoas match {
      case Some((tm, Some(h))) => h.HOMA(tm, List(t1,t2))
      case Some((tm, None)) => OMA(tm, List(t1,t2))
      case None => ???
    }

    def unapply(t: Term) = tmhoas match {
      case Some((tm, Some(h))) => t match {
        case SHTMLHoas.OmaSpine(Some(`h`), `tm`, List(a,b)) => Some((a,b))
        case _ => None
      }
      case Some((tm, None)) => t match {
        case OMA(`tm`, List(a,b)) => Some((a,b))
        case _ => None
      }
      case None => None
    }
  }
}

case class SHTMLConclusion(orig:HTMLNode) extends SHTMLNode(orig,Some("conclusion")) with Propositional {
  set_in_term
  override def copy: HTMLNode = SHTMLConclusion(orig.copy)
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
                  def doTerm(tm : Term) : Term = tm match {
                    case OMBIND(OMS(ParseResult.unknown),ctx,bd) =>
                      OMBIND(OMS(ParseResult.unknown),ctx,doTerm(bd))
                    case SHTML.implicit_binder.spine(ctx,bd) =>
                      SHTML.implicit_binder(ctx,doTerm(bd))
                    case SHTML.binder(vd,bd) =>
                      vd.tp match {
                        case Some(judgment(p)) if !bd.freeVars.contains(vd.name) && implies.tmhoas.isDefined =>
                          doTerm(bd) match {
                            case judgment(c) =>
                              judgment(implies(p,c))
                            case o =>
                              SHTML.binder(vd, o)
                          }
                        case Some(p) if !bd.freeVars.contains(vd.name) && implies.tmhoas.isDefined =>
                          implies(p,doTerm(bd))
                        case _ if forall.tmhoas.isDefined =>
                          doTerm(bd) match {
                            case judgment(c) =>
                              judgment(forall(vd, c))
                            case o =>
                              forall(vd, o)
                          }
                        case _ =>
                          SHTML.binder(vd, doTerm(bd))
                      }
                    case t =>
                      judgment(t)
                  }

                  val vars = getVariableContext.filter(state.isBound).reverse.distinctBy(_.name).reverse
                  val itp = vars.foldRight(getTerm)((vd, t) => SHTML.binder(vd, t))
                  val tp = state.applyTopLevelTerm(doTerm(itp),false)
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
case class SHTMLPremise(orig:HTMLNode) extends SHTMLNode(orig,Some("premise")) with Propositional {
  set_in_term
  override def copy: HTMLNode = SHTMLPremise(orig.copy)
  lazy val varname = this.plain.attributes.get((HTMLParser.ns_shtml, "premise")) match {
    case Some(s) if s.nonEmpty => LocalName.parse(s)
    case _ => newname(LocalName("p"))
  }

  def newname(n: LocalName): LocalName = {
    getVariableContext.variables.find(_.name == n) match {
      case Some(_) => newname(LocalName.parse(n.toString + "\'"))
      case _ => n
    }
  }

  override def onAdd: Unit = {
    super.onAdd
    reset_in_term
    sstate.foreach { state =>
      findAncestor { case hc : SHTMLGroupLike => hc}.foreach { vc =>
        val vd = VarDecl(varname,judgment(getTerm))
        state.markAsBound(vd)
        vc.variables ++= vd
      }
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

  override def onOpen: Unit = {
    super.onOpen
    open
  }

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

  override def onAdd: Unit = sstate.foreach {state =>
    findAncestor {
      case a if a.docelem.isDefined || a.contentelem.isDefined => a
    }.foreach { a =>
      val e = a.docelem.getOrElse(a.contentelem.get)
      val omss = getConstants(getTerm)
      omss.foreach(s => state.rel(ULO.crossrefs(RDFImplicits.pathToIri(e), RDFImplicits.pathToIri(s))))
    }
  }
}

case class SHTMLOMStr(orig:HTMLNode) extends SHTMLNode(orig,Some("omstr"))
  with HTMLIsTerm {
  val str = this.plain.attributes.getOrElse((HTMLParser.ns_shtml,"omstr"),"")
  override def copy: HTMLNode = SHTMLOMStr(orig.copy)

  override protected def getTermI: Option[Term] = {
    Some(StringLiterals(str))
  }
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
  removed ::= "argprecs"
  argprecs = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "argprecs"), "").split(',').toList match {
    case List("") => Nil
    case ls => ls.map(_.toInt)
  }
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
  removed ::= "argprecs"
  argprecs = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "argprecs"), "").split(',').toList match {
    case List("") => Nil
    case ls => ls.map(_.toInt)
  }
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
      val doc = state.doc
      val rtarget = Path.parseD((if (target.endsWith(".tex")) target.dropRight(4) else target) + ".omdoc",NamespaceMap.empty)
      val dref = DRef(doc.path, rtarget)
      state.rel(ULO.contains(RDFImplicits.pathToIri(doc.path),RDFImplicits.pathToIri(rtarget)))
      dref.setOrigin(GeneratedDRef)
      doSourceRef(dref)
      state.add(dref)
      state.bindings.add(ImportStep(rtarget))
    }
  }
  init
}

abstract class HTMLStatement(val kind:String,orig:HTMLNode) extends SHTMLNode(orig,Some(kind))
  with SHTMLStatement with SHTMLSymbolLike {
  var path: Option[GlobalName] = None

  removed ::= "styles"
  var styles = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "styles"), "").split(',').map(_.trim).toList
  removed ::= "fors"
  fors = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "fors"), "").split(',').flatMap(
    s => Try(Path.parseS(s.trim)).toOption
  ).toList
  removed ::= "id"
  id = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "id"), "")
  if (id.isEmpty) {
    id = this.hashCode().toHexString
    this.plain.attributes((HTMLParser.ns_shtml, "id")) = id
  }

  removed ::= "inline"
  val inline = this.plain.attributes.getOrElse((HTMLParser.ns_shtml,"inline"),"true").contains("true")

  var title : Option[SHTMLTitle] = None

  protected def copyII[A <: HTMLStatement](newst: A):A = {
    copyI(newst)
    newst.fors = fors
    newst.styles = styles
    newst.id = id
    newst.title = title.map(_.copy)
    newst
  }

  def newname(t: Theory, n: String): LocalName = {
    var start = t.path.parent.toString
    if (start.endsWith(".omdoc")) start = start.dropRight(6)
    val nname = if (n.startsWith(start)) n.drop(start.length + 1) else n
    if (t.isDeclared(LocalName.parse(nname))) {
      newname(t, nname + "\'")
    } else LocalName.parse(nname)
  }

  lazy val constantpath = sstate.flatMap{ state => findAncestor { case hl: ModuleLike if hl.language_theory.isDefined => hl.language_theory.get }.map { lt =>
    lt.path ? newname(lt,if (id == "") "statement" else id)
  }}

  override def onAdd: Unit = {
    sstate.foreach { state =>
      if (!inline) state.bindings.add(StatementStep)
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

  override def onOpen: Unit = {
    super.onOpen
    if (styles.contains("symdoc")) {
      constantpath.foreach(p => contentelem = Some(p))
    }
  }


  override def onAdd: Unit = {
    super.onAdd
    if (styles.contains("symdoc")) {
      sstate.foreach{s =>
        //val lang = findAncestor{ case hl : HasLanguage => hl.language}.getOrElse("en")
        constantpath.foreach{c =>
          val node = if (inline) {
            val cp = this.plaincopy
            cp._label = "div"
            cp.classes ::= "rustex-paragraph"
            cp.node
          } else this.plain.node
          SHTMLContentManagement.addSymdoc(c,fors,node,state.controller,s.rel)
        }
        //s.addSymdoc(fors,id,node,lang)
        s match {
          case s:SemanticState =>
            s.Search.addDefi(this)
          case _ =>
        }
      }
    }
  }
}

trait VollKIAnnotation extends SHTMLNode {
  val prefix: String
  val ulo : DatatypeProperty

  removed ::= prefix + "dimension"
  lazy val dimension = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, prefix + "dimension"), "")
  removed ::= prefix + "symbol"
  lazy val symbol = this.plain.attributes.get((HTMLParser.ns_shtml, prefix + "symbol")).map(Path.parseS(_))

  override def onAdd: Unit = {
    super.onAdd
    symbol.foreach { sym =>
      sstate.foreach { state =>
        findAncestor {
          case e: SHTMLStatement if e.contentelem.isDefined => e
          case p: SHTMLProblem if p.contentelem.isDefined => p
        }.foreach { s =>
          state.rel(new ULOStatement {
            lazy val node = org.eclipse.rdf4j.model.util.Values.bnode()

            override def triples: Seq[(Resource, IRI, Value)] = Seq(
              (RDFImplicits.pathToIri(s.contentelem.get), ulo.toIri, node),
              (node, ULO.cognitiveDimension.toIri, org.eclipse.rdf4j.model.util.Values.literal(dimension)),
              (node, ULO.crossrefs.toIri, RDFImplicits.pathToIri(sym))
            )
          })
        }
      }
    }
  }
}
case class SHTMLPrecondition(orig:HTMLNode) extends SHTMLNode(orig,Some("preconditionsymbol")) with VollKIAnnotation {
  val prefix = "precondition"
  val ulo = ULO.precondition
  override def copy: HTMLNode = SHTMLPrecondition(orig.copy)
}
case class SHTMLObjective(orig:HTMLNode) extends SHTMLNode(orig,Some("objectivesymbol")) with VollKIAnnotation {
  val prefix = "objective"
  val ulo = ULO.objective
  override def copy: HTMLNode = SHTMLObjective(orig.copy)
}

class SHTMLProblem(orig:HTMLNode) extends HTMLStatement("problem",orig) {
  override def copy = new SHTMLProblem(orig.copy)

  override lazy val constantpath = sstate.flatMap { state =>
    findAncestor { case hl: ModuleLike if hl.language_theory.isDefined => hl.language_theory.get }.map { lt =>
      lt.path ? newname(lt, if (id == "") "problem" else id)
    }
  }

  override def onOpen: Unit = {
    super.onOpen
    constantpath.foreach(p => contentelem = Some(p))
  }

  override def onAdd: Unit = {
    super.onAdd
    //sstate.foreach(_.addExample(fors, id, this.plain.node.head))
    sstate match {
      case Some(s: SemanticState) =>
        constantpath.foreach(c => SHTMLContentManagement.addProblem(c, this.plaincopy.node, s.controller, s.rel))
      case _ =>
    }
  }
}

case class SHTMLExample(orig:HTMLNode) extends HTMLStatement("example",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLExample(orig))
  }

  override def onOpen: Unit = {
    super.onOpen
    constantpath.foreach(p => contentelem = Some(p))
  }


  override lazy val constantpath = sstate.flatMap { state =>
    findAncestor { case hl: ModuleLike if hl.language_theory.isDefined => hl.language_theory.get }.map { lt =>
      lt.path ? newname(lt, if (id == "") "example" else id)
    }
  }

  override def onAdd: Unit = {
    super.onAdd
    //sstate.foreach(_.addExample(fors, id, this.plain.node.head))
    sstate match {
      case Some(s: SemanticState) =>
        constantpath.foreach(c => SHTMLContentManagement.addExample(c,fors,this.plaincopy.node,s.controller,s.rel))
        s.Search.addExample(this)
      case _ =>
    }
  }
}
case class SHTMLDefinition(orig:HTMLNode) extends HTMLStatement("definition",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLDefinition(orig))
  }

  override def onOpen: Unit = {
    super.onOpen
    constantpath.foreach(p => contentelem = Some(p))
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { s =>
      //val lang = findAncestor { case hl: HasLanguage => hl.language }.getOrElse("en")
      constantpath.foreach { c =>
        val node = if (inline) {
          val cp = this.plaincopy
          cp._label = "div"
          cp.classes ::= "rustex-paragraph"
          cp.node
        } else this.plain.node
        SHTMLContentManagement.addDefinition(c, fors, node, state.controller, s.rel)
      }
      //s.addSymdoc(fors, id, node, lang)
      sstate match {
        case Some(s: SemanticState) =>
          s.Search.addDefi(this)
        case _ =>
      }
    }
  }
}
case class SHTMLAssertion(orig:HTMLNode) extends HTMLStatement("assertion",orig) {
  override def copy: HTMLNode = {
    copyII(SHTMLAssertion(orig))
  }

  override def onOpen: Unit = {
    super.onOpen
    if (styles.contains("symdoc")) {
      constantpath.foreach(p => contentelem = Some(p))
    }
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { s =>
      //val lang = findAncestor { case hl: HasLanguage => hl.language }.getOrElse("en")
      constantpath.foreach { c =>
        val node = if (inline) {
          val cp = this.plaincopy
          cp._label = "div"
          cp.classes ::= "rustex-paragraph"
          cp.node
        } else this.plain.node
        SHTMLContentManagement.addStatement(c, fors, node, state.controller, s.rel)
      }
      //s.addSymdoc(fors, id, node, lang)
      sstate match {
        case Some(s: SemanticState) =>
          s.Search.addAssertion(this)
        case _ =>
      }
    }
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
        doSourceRef(c)
        c.metadata.update(SHTML.headterm,OMS(gn))
        sstate.get.add(c)
        c
    }
  }

  override def onOpen: Unit = {
    super.onOpen
    open
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
        SHTMLContentManagement.addMacroName(macroname, orig)
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

case class SHTMLAssignment(path: GlobalName, orig: HTMLNode) extends SHTMLNode(orig, Some("assign")) with Definitional
  with HasDefiniens {
  def copy = SHTMLAssignment(path, orig.copy)

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach { state =>
      findAncestor { case s: SHTMLMMTStructure => s }.foreach { structure =>
        defi.foreach { defi =>
          val ndefi = findAncestor { case htmld:SHTMLDefinition => htmld} match {
            case Some(_) =>
              val vars = getVariableContext.filter(state.isBound).reverse.distinctBy(_.name).reverse
              (lambda.tmhoas, vars) match {
                case (Some(_), ctx) if ctx.nonEmpty =>
                  state.applyTopLevelTerm(vars.foldRight(defi)((vd, t) => lambda(vd, t)), false)
                case _ =>
                  state.applyTopLevelTerm(defi)
              }
            case _ => state.applyTopLevelTerm(defi)
          }
          val orig = structure.getAss(path)
          val nc = Constant(orig.home, orig.name, orig.alias, orig.tp, Some(ndefi), orig.rl)
          doSourceRef(nc)
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
case class SHTMLBlindSection(orig:HTMLNode) extends SHTMLNode(orig,Some("skipsection")) {
  val lvl = this.plain.attributes.get((HTMLParser.ns_shtml, "skipsection")).map(_.trim.toInt)
  def init = sstate.foreach { state =>
    state.bindings.add(new BlindSectionStep(lvl.getOrElse(-1)))
  }

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach(_.bindings.close)
  }

  override def copy = {
    val ret = new SHTMLBlindSection(orig.copy) {
      override def init = {}
    }
    ret
  }

  init
}
case class SHTMLSection(orig: HTMLNode) extends SHTMLNode(orig,Some("section")) {
  val lvl = this.plain.attributes.get((HTMLParser.ns_shtml, "section")).map(_.trim.toInt)
  def init = sstate.foreach{state =>
    state.bindings.add(new SectionStep(lvl.getOrElse(-1)))
    state match {
      case s : SemanticState =>
        val name = plain.attributes.get((plain.namespace,"id")) match {
          case Some(id) => LocalName(id)
          case _ =>
            val namestr = this.hashCode().toHexString
            plain.attributes((plain.namespace,"id")) = namestr
            LocalName(namestr)
        }
        val nd = new Document(s.doc.path / name, SectionLevel)
        s.add(nd)
        s.openDoc(nd)
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
        title.foreach(t => s.addTitle(t) )
        s.closeDoc
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
  def copy=SHTMLFillInSol(orig.copy)
}
case class SHTMLMCB(orig:HTMLNode) extends SHTMLNode(orig,Some("multiple-choice-block")){
  def copy=SHTMLMCB(orig.copy)
}
case class SHTMLMCC(orig:HTMLNode) extends SHTMLNode(orig,Some("mcc")){
  def copy=SHTMLMCC(orig.copy)
}
case class SHTMLMCSol(orig:HTMLNode) extends SHTMLNode(orig,Some("mcc-solution")){
  def copy=SHTMLMCSol(orig.copy)
}
case class SHTMLSCB(orig:HTMLNode) extends SHTMLNode(orig,Some("single-choice-block")){
  def copy=SHTMLSCB(orig.copy)
}
case class SHTMLSCC(orig:HTMLNode) extends SHTMLNode(orig,Some("scc")){
  def copy=SHTMLSCC(orig.copy)
}
case class SHTMLSCSol(orig:HTMLNode) extends SHTMLNode(orig,Some("scc-solution")){
  def copy=SHTMLSCSol(orig.copy)
}

case class SHTMLSolution(orig: HTMLNode) extends SHTMLNode(orig,Some("solution")){
  def copy=SHTMLSolution(orig.copy)
}

case class SHTMLIfInputref(orig:HTMLNode) extends SHTMLNode(orig,Some("ifinputref")){
  def copy=SHTMLIfInputref(orig.copy)
}