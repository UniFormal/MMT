package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{AddError, ComplexStep, ContainerElement, DPath, ErrorHandler, GetError, GlobalName, LocalName, MMTTask, MPath, MutableRuleSet, Path, RuleSet, StructuralElement, utils}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, History, MMTStructureChecker, RelationHandler, Solver}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, NotFound}
import info.kwarc.mmt.api.notations.{HOAS, HOASNotation, NestedHOASNotation}
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMFOREIGN, OMPMOD, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.opaque.OpaqueXML.MMTIndex
import info.kwarc.mmt.api.opaque.{OpaqueXML, TermFragmentInXML}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, RuleConstantInterpreter, Structure, TermContainer}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.stex.Extensions.Definienda
import info.kwarc.mmt.stex.{SHTML, STeXError, STeXServer, STeXTraverser}
import info.kwarc.mmt.stex.lsp.STeXLSPErrorHandler
import info.kwarc.mmt.stex.rules.{HTMLTermRule, StringLiterals}
//import info.kwarc.mmt.stex.rules.{BindingRule, ConjunctionLike, ConjunctionRule, Getfield, HTMLTermRule, ModelsOf, ModuleType, RecType, SubstRule}
import info.kwarc.mmt.stex.search.SearchDocument
//import info.kwarc.mmt.stex.{NestedHOAS, OMDocHTML, SCtx, SOMA, SOMB, SOMBArg, STeX, STeXError, STeXHOAS, STerm, SimpleHOAS}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class SemanticState(val server:STeXServer, rules : List[HTMLRule], eh : ErrorHandler, val dpath : DPath) extends HTMLParser.ParsingState(server.ctrl,rules) with SHTMLState[SHTMLNode] {
  private var docs = List(new Document(dpath))
  def closeDoc = {
    val h :: tail = docs
    if (docs.length > 1) docs = tail
    // TODO h
  }
  def openDoc(d : Document) = {
    docs ::= d
  }

  def doc = docs.head
  var missings: List[Path] = Nil // Search Stuff

  def getRules(context:Context) = try {
    RuleSet.collectRules(controller, context)
  } catch {
    case g: GetError =>
      if (!missings.contains(g.path)) {
        eh(g)
        g.path match {
          case mp: MPath => this.missings ::= mp
          case _ =>
        }
      }
      new MutableRuleSet
  }
  def applyTerm(tm: Term)(implicit self:SHTMLNode): Term = {
    val rules = getRules(self.getRuleContext).get(classOf[HTMLTermRule]).toList.sortBy(_.priority)
    val ret = rules.foldLeft(tm)((it,f) => f.apply(it)(this,self,applyTerm(_)).getOrElse(it))
    ret match {
      case OMV(x) =>
        self.getVariableContext.findLast(_.name == x) match {
          case Some(vd) =>
            vd.metadata.getValues(SHTML.headterm).headOption match {
              case Some(OMS(p)) =>
                self.plain.attributes((HTMLParser.ns_mmt, "variable")) = p.toString
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
    ret
  }

  def applyTopLevelTerm(tm: Term,bindvars:Boolean = true)(implicit self:SHTMLNode) : Term = {
    var ntm = if (bindvars) {
      val vars = self.getVariableContext.filter(isBound)
        .reverse.distinctBy(_.name).reverse
      vars.foldRight(tm)((vd, t) => SHTML.binder(vd, t)) //SHTML.binder(vars,tm)
    } else tm

    var implbinds = Context.empty
    var tosolves = Context.empty
    var nobinds : List[LocalName] = Nil
    val trav1 = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case v@OMV(x) if isUnknown(v) =>
          tosolves ++= VarDecl(x)
          v
          //Solver.makeUnknown(x,(implbinds ++ con).map(v => OMV(v.name)))
        case v@OMV(x) if !(implbinds ++ tosolves ++ con).isDeclared(x) =>
          self.getVariableContext.findLast(_.name == x) match {
            case Some(vd) if vd.tp.isDefined =>
              vd.tp.foreach(traverse)
              vd.df.foreach(traverse)
              implbinds ++= vd
            case Some(vd) =>
              vd.df.foreach(traverse)
              val tpn = getUnknownTp
              tosolves ++= VarDecl(tpn)
              val nvd = VarDecl(vd.name,vd.feature,Some(OMV(tpn)),vd.df,vd.not)
              nvd.copyFrom(vd)
              implbinds ++= nvd
            case None =>
              val tpn = getUnknownTp
              tosolves ++= VarDecl(tpn)
              val vd = VarDecl(x, OMV(tpn))
              implbinds ++= vd//OMV(tpn))
          }
          v
        case _ => STeXTraverser(this,t)
      }
    }
    val trav2 = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMV(x) if !nobinds.contains(x) && tosolves.exists(_.name == x) =>
          Solver.makeUnknown(x,con.map(v => OMV(v.name)))
        case _ => STeXTraverser(this,t)
      }
    }
    ntm = trav1(ntm,())
    tosolves = tosolves.distinct
    implbinds = implbinds.sortBy(vd => self.getVariableContext.indexOf(vd) match {case -1 => 10000 case i => i})
    ntm = trav2(SHTML.implicit_binder(implbinds,ntm),())
    if (tosolves.isEmpty) ntm else OMBIND(OMS(ParseResult.unknown),tosolves,ntm)
  }
  def addNotation(path: GlobalName, id: String, opprec: Int,argprecs:List[Int], component: SHTMLONotationComponent, op: Option[SHTMLOOpNotationComponent])(implicit context: SHTMLNode): Unit = {
    context.findAncestor {
      case t: ModuleLike =>
        val th = t.signature_theory.getOrElse(t.language_theory.getOrElse({ return () }))
        server.addNotation(th,path,id,opprec,argprecs,component.asInstanceOf[HTMLNode].plaincopy,op.map(_.asInstanceOf[HTMLNode].plaincopy))
    }
  }

  override def addVarNotation(name:LocalName, id: String, opprec: Int,argprecs:List[Int], component: SHTMLONotationComponent, op: Option[SHTMLOOpNotationComponent])(implicit context: SHTMLNode): Unit = {
    context.getVariableContext.find(_.name == name).foreach(vd =>
      vd.metadata.getValues(SHTML.headterm).headOption match {
        case Some(OMS(p)) =>
          addNotation(p,id,opprec,argprecs,component,op)
        case _ =>
          server.addVarNotation(vd, id, opprec, argprecs, component.asInstanceOf[HTMLNode].plaincopy, op.map(_.asInstanceOf[HTMLNode].plaincopy))
      }
    )
  }
  def addSymdoc(fors : List[GlobalName],id:String,html:scala.xml.Node,language:String)(implicit context: SHTMLNode): Unit = {
    context.findAncestor {
      case t: ModuleLike =>
        val th = t.signature_theory.getOrElse(t.language_theory.getOrElse({
          return ()
        }))
        server.addSymdoc(th,fors,html,language)
    }
  }

  def addExample(fors: List[GlobalName], id: String, html: scala.xml.Node)(implicit context: SHTMLNode): Unit = {
    context.findAncestor {
      case t: ModuleLike =>
        val th = t.signature_theory.getOrElse(t.language_theory.getOrElse({
          return ()
        }))
        server.addExample(th, fors, html)
    }
  }

  def addTitle(ttl: SHTMLNode) : Unit = {
    val n = if (ttl.plain.attributes.contains((HTMLParser.ns_shtml,"visible"))) {
      val m = ttl.plaincopy
      m.attributes.remove((HTMLParser.ns_shtml,"visible"))
      m.attributes.remove((m.namespace,"style"))
      m
    } else ttl
    if (doctitle.isEmpty) {
      doctitle = Some(n)
    }
    server.addTitle(doc,n.plain.node.head)
  }
  private var doctitle: Option[HTMLNode] = None

  def addMissing(p : Path) = missings ::= p
  def add(se : StructuralElement) = try {controller.library.add(se)} catch {
    case NotFound(p,f) =>
      error("Not found " + p.toString)
    case AddError(e,msg) if msg.startsWith("redundancy, an equivalent declaration") =>
    case AddError(e,msg) =>
      error("Error adding " + e.path.toString + ": " + msg)
  }
  def endAdd[T <: StructuralElement](ce: ContainerElement[T]) = try {controller.library.endAdd(ce)} catch {
    case NotFound(p, f) =>
      error("Not found " + p.toString)
    case AddError(e, msg) =>
      error("Error adding " + e.path.toString + ": " + msg)
  }

  def update(se : StructuralElement) = try {
    controller.library.update(se)
  } catch {
    case NotFound(p, f) => error("Not found " + p.toString)
    case AddError(e, msg) => error("Error adding " + e.path.toString + ": " + msg)
  }
  def getO(p : Path) = try { controller.getO(p) } catch {
    case e : info.kwarc.mmt.api.Error =>
      error(e)
      None
    case ne:NoClassDefFoundError =>
      error(ne.getMessage)
      None
  }
  override def error(s: String): Unit = eh(new STeXError(s, None, None))
  override def error(e: info.kwarc.mmt.api.Error): Unit = eh(e)

  private var maindoc: SHTMLDocument = null
  override protected def onTop(n: HTMLNode): Option[HTMLNode] = {
    val nn = SHTMLDocument(dpath,n)
    maindoc = nn
    nn.replace(n)
    Some(nn)
  }

  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  private lazy val ce = CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore, new MMTTask {})

  def check(se: StructuralElement) = try {
    eh match {
      case STeXLSPErrorHandler(_, cont) =>
        cont(0.5, "Checking " + se.path.toString)
      case _ =>
    }
    checker(se)(ce)
  } catch {
    case g: GetError =>
      if (!missings.contains(g.path)) {
        eh(g)
        g.path match {
          case mp: MPath => this.missings ::= mp
          case _ =>
        }
      }
    case e: info.kwarc.mmt.api.Error =>
      eh(e)
    case e: Throwable =>
      eh.apply(new api.Error(e.getMessage) {}.setCausedBy(e))
  }

  object Search {
    def makeDocument(outdir: File, source: File, archive: Archive) = {
      val doc = new SearchDocument(outdir, source, archive, dpath)
      val body = maindoc.get()()("rustex-body").head
      doc.add("content", makeString(body), body.children.map(_.toString).mkString, doctitle.toList.flatMap { t =>
        val nt = t.copy
        nt.plain.attributes.remove((nt.namespace, "style"))
        List(("title", makeString(nt)), ("titlesource", nt.toString))
      }: _*)
      definitions.foreach(d => doc.add("definition", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.map(gn => gn.module.name.toString + "?" + gn.name.toString).mkString(", ")) ::
          d._1.path.map(p => ("path", p.toString)).toList ::: (d._2 match {
          case Some(mp) => List(("module",mp.toString))
          case _ => Nil
        }): _*
      ))
      assertions.foreach(d => doc.add("assertion", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.map(gn => gn.module.name.toString + "?" + gn.name.toString).mkString(", ")) ::
          d._1.path.map(p => ("path", p.toString)).toList ::: (d._2 match {
          case Some(mp) => List(("module", mp.toString))
          case _ => Nil
        }): _*
      ))
      examples.foreach(d => doc.add("example", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.map(gn => gn.module.name.toString + "?" + gn.name.toString).mkString(", ")) ::
          d._1.path.map(p => ("path", p.toString)).toList ::: (d._2 match {
          case Some(mp) => List(("module", mp.toString))
          case _ => Nil
        }): _*
      ))
      doc
    }

    def addDefi(df: HTMLStatement)(implicit self:SHTMLNode) = {
      val mod = self.findAncestor {
        case t: SHTMLOTheory => t.mp
      }
      docs.headOption match {
        case Some(d) if df.id.nonEmpty && df.fors.nonEmpty =>
          add(Definienda.Def(d.path,df.id,df.fors))
        case _ =>
      }
      definitions ::= (df,mod)
    }

    def addAssertion(ass: HTMLStatement)(implicit self:SHTMLNode) = {
      val mod = self.findAncestor {
        case t: SHTMLOTheory => t.mp
      }
      assertions ::= (ass,mod)
    }

    def addExample(ex: HTMLStatement)(implicit self:SHTMLNode) = {
      val mod = self.findAncestor {
        case t: SHTMLOTheory => t.mp
      }
      examples ::= (ex,mod)
    }

    private var definitions: List[(HTMLStatement,Option[MPath])] = Nil
    private var assertions: List[(HTMLStatement,Option[MPath])] = Nil
    private var examples: List[(HTMLStatement,Option[MPath])] = Nil

    private def makeString(node: HTMLNode): String = {
      val sb = new mutable.StringBuilder()
      recurse(node)(sb)
      sb.mkString.trim
    }

    @tailrec
    private def recurse(node: HTMLNode)(implicit sb: mutable.StringBuilder): Unit = {
      node match {
        case txt: HTMLText =>
          sb ++= txt.toString().trim + " "
        case _ =>
      }
      (node.plain.attributes.get((node.namespace, "style")) match {
        case Some(s) if s.replace(" ", "").contains("display:none") =>
          getNext(node, false)
        case _ => getNext(node)
      }) match {
        case Some(s) => recurse(s)
        case _ =>
      }
    }

    private def getNext(node: HTMLNode, withchildren: Boolean = true): Option[HTMLNode] = node.children match {
      case h :: _ if withchildren => Some(h)
      case _ => node.plain.parent match {
        case Some(p) =>
          val children = p.children
          children.indexOf(node) match {
            case i if i != -1 && children.isDefinedAt(i + 1) => Some(children(i + 1))
            case _ => getNext(p, false)
          }
        case _ => None
      }
    }
  }
}

class SearchOnlyState(server:STeXServer, rules : List[HTMLRule],eh : ErrorHandler, dpath : DPath) extends SemanticState(server,rules,eh,dpath) {
  override def add(se : StructuralElement) = {}
  override def endAdd[T <: StructuralElement](ce: ContainerElement[T]) = {}
  override def update(se: StructuralElement): Unit = {}
  override def applyTerm(tm: Term)(implicit self: SHTMLNode): Term = tm
  override def applyTopLevelTerm(tm: Term,bindvars:Boolean = true)(implicit self: SHTMLNode): Term = tm
  override def check(se: StructuralElement): Unit = {}
}