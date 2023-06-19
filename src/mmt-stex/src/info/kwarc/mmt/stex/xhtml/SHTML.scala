package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMMOD, OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{RDFImplicits, ULO, ULOStatement}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule, PlainInclude, RuleConstant, RuleConstantInterpreter, Structure}
import info.kwarc.mmt.api.{ComplexStep, ContainerElement, ContentPath, DPath, ElaborationOf, GeneratedFrom, GetError, GlobalName, LocalName, MPath, ParametricRule, Path, Rule, RuleSet, StructuralElement}
import info.kwarc.mmt.stex.Extensions.{LateBinding, SHTMLContentManagement, STeXRelationals}
import info.kwarc.mmt.stex.{IsSeq, SCtx, SHTML, SHTMLHoas, STeXServer, STerm}
import info.kwarc.mmt.stex.rules.{BinRRule, BindingRule, ConjRule, ModelsOf, PreEqualRule, PreRule, Reorder, RulerRule, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.collection.mutable

trait SHTMLObject {
  type SHTMLClass <: SHTMLObject

  var docelem:Option[DPath] = None
  var contentelem:Option[ContentPath] = None

  implicit val self : SHTMLClass = this.asInstanceOf[SHTMLClass]
  def sstate : Option[SHTMLState[SHTMLClass]]
  def findAncestor[A](f: PartialFunction[SHTMLClass, A]): Option[A]
  def doSourceRef(hm : HasMetaData) : Unit
  def getTerm : Term
  protected def getTermI : Option[Term]

  def matchterms(df : Option[Term], tp:Option[Term]) : (Option[Term],Option[Term]) = (df,tp) match {
    case (Some(_),None) | (None,Some(_)) | (None,None) => (df,tp)
    case (Some(OMBIND(OMS(ParseResult.unknown), ct, tm)),_) =>
      matchterms(Some(tm),tp) match {
        case (Some(itm),tp) => (Some(OMBIND(OMS(ParseResult.unknown),ct,itm)),tp)
        case _ => (df,tp) // impossible
      }
    case (_,Some(OMBIND(OMS(ParseResult.unknown), ct, tm))) =>
      matchterms(df,Some(tm)) match {
        case (df,Some(itm)) => (df,Some(OMBIND(OMS(ParseResult.unknown),ct,itm)))
        case _ => (df,tp) // impossible
      }
    case (Some(SHTML.implicit_binder.spine(ctxa,idf)),Some(SHTML.implicit_binder.spine(ctxb,itp))) =>
      import info.kwarc.mmt.api.objects.Conversions._
      if (ctxa.length == ctxb.length) {
        (df,tp)
      } else if (ctxb.forall(ctxa.contains)) {
        val ntp = SHTML.implicit_binder(ctxa,itp)
        (df,Some(ntp))
      } else if (ctxa.forall(ctxb.contains)) {
        val ndf = SHTML.implicit_binder(ctxb,idf)
        (Some(ndf),tp)
      } else {
        (df,tp)
      }
    case (Some(SHTML.implicit_binder.spine(ctxa, _)), Some(tp)) =>
      (df,Some(SHTML.implicit_binder(ctxa,tp)))
    case (Some(df), Some(SHTML.implicit_binder.spine(ctxb, _))) =>
      (Some(SHTML.implicit_binder(ctxb,df)),tp)
    case (_,_) => (df,tp)
  }

  def getVariableContext : Context = findAncestor{
    case gl:SHTMLGroupLike => gl.getVariables
  }.getOrElse(Context.empty)
  def getRuleContext : Context = {
    val maybe = findAncestor {
      case hrc: HasRuleContext => hrc.context
    }.getOrElse(Context.empty)
    if (maybe.isEmpty) {
      findAncestor {
        case doc : SHTMLODocument =>
          doc.language_theory match {
            case Some(o) => Context(o.path)
            case _ => maybe
          }
      }
    }.getOrElse(maybe) else maybe
  }

  def getMetaTheory = {
    this match {
      case th: SHTMLOTheory if th.metatheory.isDefined =>
        th.metatheory
      case _ =>
        findAncestor {
          case th: SHTMLOTheory if th.metatheory.isDefined =>
            th.metatheory.get
        }
    }
  }
}

trait SHTMLState[SHTMLClass <: SHTMLObject] {
  val server : STeXServer
  var in_term : Boolean = false
  val bindings = new LateBinding
  val rel:ULOStatement => Unit

  def closeDoc : Unit

  def openDoc(d: Document) : Unit
  def addTitle(ttl:SHTMLClass) : Unit
  private var term_num = 0
  def termname = {
    term_num += 1
    LocalName("term " + term_num.toString)
  }
  def getO(p : Path) : Option[StructuralElement]
  def addMissing(p : Path)
  def add(se : StructuralElement) : Unit
  def endAdd[T <: StructuralElement](ce: ContainerElement[T]) : Unit

  def getRuler(tm:Term)(implicit self:SHTMLClass) : Option[HasMetaData] = {
    SHTMLContentManagement.getRuler(tm,self.getRuleContext ++ self.getVariableContext)(server.ctrl)
  }
  def update(se: StructuralElement)
  def error(s:String): Unit
  def error(e:info.kwarc.mmt.api.Error) : Unit
  def doc : Document
  def applyTerm(tm : Term)(implicit self:SHTMLClass): Term
  def applyTopLevelTerm(tm : Term,bindvars:Boolean = true)(implicit self:SHTMLClass): Term
  def check(se: StructuralElement) : Unit

  private lazy val rci = new RuleConstantInterpreter(server.ctrl)
  def addRule(parent:MPath,ruleterm:Term)(implicit self:SHTMLClass) = try {
    val rule = rci(parent,ruleterm,true)
    self.doSourceRef(rule)
    add(rule)
  } catch {
    case e:info.kwarc.mmt.api.Error => error(e)
  }

  def getMetaRules[A <: Rule](cls:Class[A])(implicit self:SHTMLClass) : List[A] = {
    self.getMetaTheory match {
      case Some(mp) =>
        try {RuleSet.collectRules(server.ctrl,Context(mp)).get(cls).toList} catch {
          case _: GetError =>
            Nil
        }
      case _ => Nil
    }
  }

  def makeBinder(tm : Term,assoc:Boolean)(implicit self:SHTMLClass) : Context = {
    val rules = try {RuleSet.collectRules(server.ctrl,self.getRuleContext).get(classOf[BindingRule]) } catch {
      case _: GetError =>
        return VarDecl(LocalName("argh"), df = tm)
    }
    rules.foreach{ r =>
      val ret = r(tm,assoc)(this,self)
      ret match {
        case Some(ctx) => return ctx
        case _ =>
      }
    }
    tm match {
      case SHTML.flatseq(seq) =>
        var retctx = Context.empty
        seq.foreach {tm =>
          var done = false
          rules.foreach { r =>
            if (!done) {
              val ret = r(tm, assoc)(this, self)
              ret match {
                case Some(ctx) => retctx ++= ctx
                  done = true
                case _ =>
              }
            }
          }
          if (!done)
            retctx ++= VarDecl(LocalName("argh"), df = tm)
        }
        retctx
      case _ =>
        VarDecl(LocalName("argh"), df = tm)
    }
  }
  def addNotation(path: GlobalName, id:String,opprec:Int,argprecs:List[Int],component:SHTMLONotationComponent,op:Option[SHTMLOOpNotationComponent])(implicit context : SHTMLClass) : Unit

  def addVarNotation(name:LocalName, id: String, opprec: Int,argprecs:List[Int], component: SHTMLONotationComponent, op: Option[SHTMLOOpNotationComponent])(implicit context: SHTMLClass): Unit

  def markAsUnknown(v: OMV): OMV = {
    v.metadata.update(ParseResult.unknown, OMS(ParseResult.unknown))
    v
  }
  def markAsBound(vd : VarDecl) : VarDecl = {
    vd.metadata.update(SHTML.meta_quantification,OMS(SHTML.meta_quantification))
    vd
  }
  def isBound(vd : VarDecl) : Boolean = {
    vd.metadata.getValues(SHTML.meta_quantification).contains(OMS(SHTML.meta_quantification))
  }

  def isUnknown(v: OMV) = {
    v.metadata.getValues(ParseResult.unknown).contains(OMS(ParseResult.unknown))
  }
  private var unknowns_counter = 0
  def getUnknown = {
    unknowns_counter += 1
    LocalName("") / "i" / unknowns_counter.toString
  }
  def getUnknownTp = {
    unknowns_counter += 1
    LocalName("") / "I" / unknowns_counter.toString
  }
  def getUnknownWithTp = {
    unknowns_counter += 1
    (LocalName("") / "i" / unknowns_counter.toString, LocalName("") / "I" / unknowns_counter.toString)
  }
}

trait ModuleLike extends SHTMLObject with HasRuleContext {
  def mp : MPath
  var signature_theory: Option[Theory] = None
  var language_theory: Option[Theory] = None
}
trait SHTMLODocument extends SHTMLObject with SHTMLGroupLike with HasLanguage with ModuleLike {
  val path:DPath
  def mp = path ? LocalName(language)
  var doc: Option[Document] = None

  sstate.foreach { s =>
    if (path == s.doc.path) {
      doc = Some(s.doc)
      docelem = Some(s.doc.path)
      s.add(s.doc)
    } else {
      val d = new Document(path)
      doc = Some(d)
      docelem = Some(d.path)
      s.add(d)
    }
  }

  def open: Unit = sstate.foreach { state =>
    if (language.isEmpty) language = "en"
    //if (language != "") {
    val lang = Theory(path, LocalName(language), None)
    SHTMLContentManagement.addLanguage(language, lang)
    doSourceRef(lang)
    language_theory = Some(lang)
    contentelem = Some(lang.path)
    state.add(lang)
    state.rel(ULO.has_language(RDFImplicits.pathToIri(lang.path),language))
    //STeXRelationals.has_language(lang.path,language)
    docelem.foreach{d =>
      if (d.last.endsWith(s".$language.omdoc")) {
        val od = d.^ / (d.last.dropRight(s".$language.omdoc".length) + ".omdoc")
        state.rel(ULO.has_language_module(RDFImplicits.pathToIri(od),RDFImplicits.pathToIri(lang.path)))
      }
      state.add(MRef(d,lang.path))
      state.rel(ULO.document(RDFImplicits.pathToIri(d)))
      state.rel(ULO.theory(RDFImplicits.pathToIri(lang.path)))
      state.rel(ULO.is_language_module(RDFImplicits.pathToIri(lang.path)))
    }
  }
  def close: Unit = {
    language_theory.foreach(t => sstate.foreach { s => s.endAdd(t) })
  }
}
trait SHTMLOVisible extends SHTMLObject {
  val visible : Boolean
}

trait HasLanguage extends SHTMLObject {
  var language : String = ""
}
trait SHTMLGroupLike extends SHTMLObject {
  var variables = Context.empty
  def getVariables : Context = findAncestor {
    case gl:SHTMLGroupLike => gl
  }.map(_.getVariables).getOrElse(Context.empty) ++ variables
}

trait HasRuleContext extends SHTMLObject with SHTMLGroupLike {
  protected var _context = Context.empty
  def context : Context = sstate.map(_ => findAncestor{ case th:HasRuleContext => th}.map(_.context).getOrElse(Context.empty) ++ _context).getOrElse(Context.empty)
  def addToContext(ctx : Context) = _context = _context ++ ctx
}

trait SHTMLOTheory extends HasLanguage with ModuleLike {
  val mp : MPath
  var metatheory: Option[MPath] = None
  var signature: String = ""

  lazy val hoas : Option[SHTMLHoas.HoasRule] = sstate.flatMap(_.getMetaRules(classOf[SHTMLHoas.HoasRule]).headOption)

  def open: Unit = sstate.foreach { state =>
    val pth = findAncestor { case th: ModuleLike if th.signature_theory.isDefined => th.signature_theory.get }
    val plang = findAncestor { case th:ModuleLike if th.language_theory.isDefined => th.language_theory.get }
    metatheory.foreach { p =>
      state.getO(p) match {
        case None =>
          state.addMissing(p)
          metatheory = None
        case _ =>
      }
    }
    pth match {
      case Some(t) =>
        val th = Theory(mp.parent, mp.name, metatheory) // TODO parameters
        doSourceRef(th)
        val nt = new NestedModule(t.toTerm, mp.name.tail, th)
        state.add(nt)
        signature_theory = Some(th)
        plang.foreach{lang =>
          val incl = PlainInclude(mp, lang.path)
          //incl.setOrigin(GeneratedFrom(lang.path,this))
          state.add(incl)
          state.endAdd(incl)
          state.rel(ULO.includes(RDFImplicits.pathToIri(lang.path),RDFImplicits.pathToIri(mp)))
        }
        contentelem = Some(th.path)
        state.rel(ULO.theory(RDFImplicits.pathToIri(th.path)))
        metatheory.foreach(p => state.rel(ULO.has_meta_theory(RDFImplicits.pathToIri(mp), RDFImplicits.pathToIri(p))))
        state.rel(ULO.contains(RDFImplicits.pathToIri(t.path),RDFImplicits.pathToIri(mp)))
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(mp.parent, mp.name, metatheory) // TODO parameters
      doSourceRef(th)
      signature_theory = Some(th)
      state.add(th)
      plang.foreach { lang =>
        val incl = PlainInclude(mp, lang.path)
        //incl.setOrigin(GeneratedFrom(lang.path,this))
        state.add(incl)
        state.endAdd(incl)
        state.rel(ULO.includes(RDFImplicits.pathToIri(lang.path),RDFImplicits.pathToIri(mp)))
      }

      contentelem = Some(th.path)
      state.rel(ULO.theory(RDFImplicits.pathToIri(th.path)))
      metatheory.foreach(p => state.rel(ULO.has_meta_theory(RDFImplicits.pathToIri(mp), RDFImplicits.pathToIri(p))))
      state.rel(ULO.contains(RDFImplicits.pathToIri(state.doc.path), RDFImplicits.pathToIri(mp)))
    } else if (language == signature) {
      val sig = Theory(mp.parent, mp.name, metatheory) // TODO parameters
      doSourceRef(sig)
      signature_theory = Some(sig)
      state.add(sig)
      plang.foreach { lang =>
        val incl = PlainInclude(mp, lang.path)
        //incl.setOrigin(GeneratedFrom(lang.path,this))
        state.add(incl)
        state.endAdd(incl)
        state.rel(ULO.includes(RDFImplicits.pathToIri(lang.path), RDFImplicits.pathToIri(mp)))
      }
      contentelem = Some(sig.path)
      state.rel(ULO.theory(RDFImplicits.pathToIri(sig.path)))
      metatheory.foreach(p => state.rel(ULO.has_meta_theory(RDFImplicits.pathToIri(mp), RDFImplicits.pathToIri(p))))
      state.rel(ULO.contains(RDFImplicits.pathToIri(state.doc.path), RDFImplicits.pathToIri(mp)))
    } else {
      plang.foreach { lang =>
        val incl = PlainInclude(mp, lang.path)
        state.add(incl)
        state.endAdd(incl)
        state.check(incl)
        state.rel(ULO.includes(RDFImplicits.pathToIri(lang.path), RDFImplicits.pathToIri(mp)))
      }
    }
    (signature_theory.toList ::: language_theory.toList).foreach { t =>
      state.add(MRef(state.doc.path,t.path))
      state.check(t)
    }
    (plang,language_theory) match {
      case (Some(t),Some(lang)) =>
        val nt = new NestedModule(t.toTerm, LocalName(lang.name), lang)
        state.add(nt)
      case _ =>
    }
  }
  def close: Unit = {
    signature_theory.foreach(t => sstate.foreach { s => s.endAdd(t) })
    language_theory.foreach(t => sstate.foreach { s => s.endAdd(t) })
  }
}
trait SHTMLOImportModule extends SHTMLObject {
  val mp : MPath
  def close: Unit = {
    sstate.foreach { state =>
      findAncestor { case ml: ModuleLike => ml }.foreach { ml =>
        ml.signature_theory.foreach { th =>
          val pl = PlainInclude(mp, th.path)
          doSourceRef(pl)
          state.add(pl)
          state.endAdd(pl)
          state.rel(ULO.includes(RDFImplicits.pathToIri(th.path), RDFImplicits.pathToIri(mp)))
          //state.check(pl)
        }
      }
    }

  }
}
trait SHTMLOUseModule extends SHTMLObject {
  val mp : MPath
  def close: Unit = {
    sstate.foreach { state =>
      findAncestor { case ml: ModuleLike if ml.language_theory.isDefined => ml.language_theory.get }.foreach { th =>
        val pl = PlainInclude(mp, th.path)
        doSourceRef(pl)
        state.add(pl)
        state.endAdd(pl)
        //state.check(pl)
        state.rel(ULO.includes(RDFImplicits.pathToIri(th.path), RDFImplicits.pathToIri(mp)))
      }
    }
  }
}
trait HasTypes extends SHTMLObject {
  var types : List[Term] = Nil
}
trait HasDefiniens extends SHTMLObject {
  var defi: Option[Term] = None
}
trait HasMacroName extends SHTMLObject {
  var macroname = ""
}
trait HasRoles extends SHTMLObject {
  var roles:List[String] = Nil
}

trait SymbolLike extends HasTypes with HasDefiniens with HasMacroName with HasRoles {
  var return_type: Option[Term] = None
  var reorderargs = ""
  var assoctype = ""
  var args = ""

  def getType = return_type match {
    case None =>
      types match {
        case List(a) => Some(a)
        case Nil => None
        case _ =>
          ???
      }
    case Some(rt) =>
      types match {
        case Nil if args.isEmpty => Some(rt)
        case ls if ls.length == args.length =>
          import info.kwarc.mmt.api.objects.Conversions._
          // TODO choose binder
          val ret = SHTML.binder(ls.zipWithIndex.map{case (t,i) => LocalName("ARGUMENT_" + (i+1)) % t},rt)
          Some(ret)
        case _ =>
          Some(rt)
      }
  }

  def addRule(c: Constant, name: String, rule: ParametricRule, args: List[Term]): Unit = sstate.foreach { state =>
    val rl = rule.mpath
    val rc = RuleConstant(c.home, LocalName(c.name.toString + "_$_" + name), OMA(OMMOD(rl), args), None)
    state.add(rc)
    state.check(rc)
  }
}
trait SHTMLOSymbol extends SymbolLike {
  val path : GlobalName
  def open = {
    sstate.foreach {state => state.rel(ULO.constant(RDFImplicits.pathToIri(path)))}
    contentelem = Some(path)
  }
  def close: Unit = {
    sstate.foreach { state =>
      val itp = getType.map(state.applyTopLevelTerm(_))
      val idf = defi.map(state.applyTopLevelTerm(_))
      val (df,tp) = matchterms(idf,itp)
      val rl = roles match {
        case Nil => None
        case ls => Some(ls.mkString(" "))
      }
      val hoas = findAncestor {
        case th : SHTMLOTheory => th.hoas
      }.flatten
      val c = Constant(OMMOD(path.module), path.name, Nil, tp, df, rl)
      hoas.foreach(_.apply(c))
      if (macroname.nonEmpty) SHTMLContentManagement.addMacroName(macroname,c)
      if (args.nonEmpty) SHTMLContentManagement.addArity(args,c)
      if (assoctype.nonEmpty) SHTMLContentManagement.addAssoctype(assoctype, c)
      if (reorderargs.nonEmpty) SHTMLContentManagement.addReorder(reorderargs, c)
      doSourceRef(c)
      state.add(c)
      state.check(c)
    }
  }
}
trait SHTMLOVarDecl extends SymbolLike with HasMacroName {
  val name : LocalName
  var bind = false

  private val path = findAncestor { case o: ModuleLike if o.language_theory.isDefined => o.language_theory.get } map { t =>
    t.path ? newname(t, name)
  }
  def open = path.foreach { path =>
    sstate.foreach { state => state.rel(ULO.variable(RDFImplicits.pathToIri(path))) }
    contentelem = Some(path)
  }

  def closeSeq = {
    sstate.foreach { state =>
      val tp = getType match {
        case Some(OMV(x)) => getVariableContext.findLast(_.name == x) match {
          case Some(vd) if IsSeq(vd) => Some(OMV(x))
          case _ => Some(SHTML.flatseq.tp(OMV(x)))
        }
        case Some(o) => Some(SHTML.flatseq.tp(o))
        case o => o
      }//.map(state.applyTopLevelTerm)
      val df = defi//.map(state.applyTopLevelTerm)
      val rl = roles match {
        case Nil => None
        case ls => Some(ls.mkString(" "))
      }
      // TODO
      findAncestor { case o: SHTMLGroupLike => o }.foreach { gr =>
        val hoas = findAncestor {
          case th: SHTMLOTheory => th.hoas
        }.flatten
        val vd = VarDecl(name, None, tp, df, None)
        findAncestor { case o: ModuleLike if o.language_theory.isDefined => o.language_theory.get } match {
          case Some(t) =>
            val cname = path.get.name
            val c = Constant(t.toTerm, cname, Nil, tp, df, Some(("variable" :: rl.toList).mkString(" ")) )
            hoas.foreach(_.apply(c))
            if (macroname.nonEmpty) SHTMLContentManagement.addMacroName(macroname, c)
            if (args.nonEmpty) SHTMLContentManagement.addArity(args, c)
            if (assoctype.nonEmpty) SHTMLContentManagement.addAssoctype(assoctype, c)
            if (reorderargs.nonEmpty) SHTMLContentManagement.addReorder(reorderargs, c)
            SHTMLContentManagement.setHead(vd,c.toTerm)
            doSourceRef(c)
            state.add(c)
            //state.check(c)
          case None =>
            hoas.foreach(_.apply(vd))
            if (macroname.nonEmpty) SHTMLContentManagement.addMacroName(macroname, vd)
            if (args.nonEmpty) SHTMLContentManagement.addArity(args, vd)
            if (assoctype.nonEmpty) SHTMLContentManagement.addAssoctype(assoctype, vd)
            if (reorderargs.nonEmpty) SHTMLContentManagement.addReorder(reorderargs, vd)
        }
        IsSeq.set(vd)
        doSourceRef(vd)
        if (bind) state.markAsBound(vd)
        gr.variables ++= vd
      }
    }
  }
  def newname(t : Theory, n : LocalName) : LocalName = {
    if (t.isDeclared(n)) {
      newname(t,LocalName.parse(n.toString + "\'"))
    } else n
  }
  def close: Unit = {
    sstate.foreach { state =>
      val tp = getType//.map(state.applyTopLevelTerm)
      val df = defi//.map(state.applyTopLevelTerm)
      val rl = roles match {
        case Nil => None
        case ls => Some(ls.mkString(" "))
      }
      findAncestor{case o: SHTMLGroupLike => o}.foreach{gr =>
        val hoas = findAncestor {
          case th: SHTMLOTheory => th.hoas
        }.flatten
        val vd = VarDecl(name,None,tp,df,None)
        findAncestor { case o: ModuleLike if o.language_theory.isDefined => o.language_theory.get } match {
          case Some(t) =>
            val cname = path.get.name
            val c = Constant(t.toTerm, cname, Nil, tp, df, Some(("variable" :: rl.toList).mkString(" ")))
            hoas.foreach(_.apply(c))
            if (macroname.nonEmpty) SHTMLContentManagement.addMacroName(macroname, c)
            if (args.nonEmpty) SHTMLContentManagement.addArity(args, c)
            if (assoctype.nonEmpty) SHTMLContentManagement.addAssoctype(assoctype, c)
            if (reorderargs.nonEmpty) SHTMLContentManagement.addReorder(reorderargs, c)
            SHTMLContentManagement.setHead(vd,c.toTerm)
            doSourceRef(c)
            state.add(c)
            //state.check(c)
          case None =>
            hoas.foreach(_.apply(vd))
            if (macroname.nonEmpty) SHTMLContentManagement.addMacroName(macroname, vd)
            if (args.nonEmpty) SHTMLContentManagement.addArity(args, vd)
            if (assoctype.nonEmpty) SHTMLContentManagement.addAssoctype(assoctype, vd)
            if (reorderargs.nonEmpty) SHTMLContentManagement.addReorder(reorderargs, vd)
        }
        doSourceRef(vd)
        if (bind) state.markAsBound(vd)
        gr.variables ++= vd
      }
    }
  }
}

trait HasTerm extends SHTMLObject {
  var term : Option[Term] = None
}
trait IsTerm extends SHTMLObject with HasTerm {
  val notationid : String
  def headsymbol:Option[Term]
  def head = {
    val ret = term match {
      case Some(tm) =>
        Some(tm)
      case _ =>
        headsymbol
    }
    ret match {
      case Some(r) => r
      case None =>
        ???
    }
  }
}
trait IsTermWithArgs extends IsTerm {
  var args = mutable.HashMap.empty[Int,(Char,List[Term])]
  def getArgs = args.toList.sortBy(_._1.toString).map{
    case (_,(c@('i'|'b'),List(a))) => (c,a)
    case (_,(c@('a'|'B'),ls)) => ls match {
      case List(t@OMV(v)) =>
        getVariableContext.find(_.name == v) match {
          case Some(vd) if IsSeq(vd) =>
            (c, t)
          case _ =>
            (c, SHTML.flatseq(ls))
        }
      case _ =>
        (c, SHTML.flatseq(ls))
    }
    case _ =>
      ???
  }
}
trait SHTMLOArg extends SHTMLObject {
  val ind:Int
  def argmode : String
  def close: Unit = {
    val iind = ind.toString.head.toString.toInt
    findAncestor { case t: IsTermWithArgs => t }.foreach { t =>
      (argmode, t.args.get(iind)) match {
        case (s, Some(ls)) if s.startsWith("a") | s.startsWith("B") =>
          t.args(iind) = (argmode.head, ls._2 ::: getTerm :: Nil)
        case (s, None) if s.startsWith("a") | s.startsWith("B") =>
          t.args(iind) = (argmode.head, getTerm :: Nil)
        case _ => t.args(iind) = (argmode.head, List(getTerm))
      }
    }
  }
}
trait SHTMLOTopLevelTerm extends SHTMLObject {

  def open = contentelem = sstate.flatMap { state =>
      findAncestor { case m: ModuleLike if m.language_theory.isDefined => m }.map { mod =>
        mod.language_theory.get.path ? state.termname
      }
    }

  def close = {
    sstate.flatMap { state =>
      contentelem.foreach { c =>
        findAncestor {
          case a if a.docelem.isDefined || a.contentelem.isDefined => a
        }.foreach { a =>
          val e = a.docelem.getOrElse(a.contentelem.get)
          state.rel(ULO.contains(RDFImplicits.pathToIri(e), RDFImplicits.pathToIri(c)))
        }
      }
      findAncestor{case m : ModuleLike if m.language_theory.isDefined => m}.flatMap {mod =>
        getTerm match {
          case OMS(_)|OMV(_) => None
          case SHTMLHoas.OmaSpine(_,_,args) if args.forall(a => a.isInstanceOf[OMV] && state.isUnknown(a.asInstanceOf[OMV])) => None
          case o =>
            val df = state.applyTopLevelTerm(o)
            val c = Constant(OMMOD(mod.language_theory.get.path), contentelem.get.name, Nil, None, Some(df), Some("mmt_term"))
            doSourceRef(c)
            state.add(c)
            state.check(c)
            Some(c)
        }
      }
    }
  }
}
trait SHTMLOOMB extends IsTermWithArgs {
  override def getTermI: Option[Term] = {
    val hoas = sstate.flatMap(_.getRuler(head).flatMap(SHTMLHoas.get))
    val args = getArgs.map {
      case ('i' | 'a', t) => STerm(t)
      case (c@('b' | 'B'), tms) => sstate.map(s => SCtx(s.makeBinder(tms, c == 'B'))).getOrElse(STerm(tms))
    }

    val ret = hoas match {
      case Some(h) => h.HOMB(head,args)
      case _ => OMBINDC(head,args.foldLeft(Context.empty){
          case (ctx,SCtx(ctx2)) => ctx ++ ctx2
          case (ctx,_) => ctx
        },args.collect {
          case STerm(t) => t
        })
    }
    doSourceRef(ret)
    if (!headsymbol.contains(head)) {
      headsymbol.foreach{h => SHTMLContentManagement.setHead(ret,h) }
    }
    Some(ret)
  }
}
trait SHTMLOOMA extends IsTermWithArgs {
  override def getTermI: Option[Term] = {
    val hoas = sstate.flatMap(_.getRuler(head).flatMap(SHTMLHoas.get))
    val args = getArgs.map(_._2)
    val ret = hoas match {
      case Some(h) => h.HOMA(head,args)
      case _ => OMA(head,args)
    }
    doSourceRef(ret)
    if (!headsymbol.contains(head)) {
      headsymbol.foreach { h => SHTMLContentManagement.setHead(ret,h) }
    }
    Some(ret)
  }
}
trait SHTMLOMIDorOMV extends IsTerm {
  override def getTermI : Option[Term] = {
    val ret = head
    doSourceRef(ret)
    if (!headsymbol.contains(head)) {
      headsymbol.foreach { h => SHTMLContentManagement.setHead(ret,h) }
    }
    Some(head)
  }
}
trait SHTMLONotation extends SHTMLObject {
  var id : String = ""
  var opprec : Int = 0
  var component: Option[SHTMLONotationComponent] = None
  var opcomponent: Option[SHTMLOOpNotationComponent] = None
  var argprecs : List[Int] = Nil
  def close(path: GlobalName): Unit = {
    sstate.foreach(s => component.foreach { c =>
      val npath = s.getO(path).flatMap{_.path match {
        case gn:GlobalName =>
          Some(gn)
        case _ => None
      }}.getOrElse(path)
      s.addNotation(npath, id, opprec, argprecs,c, opcomponent)
    })
  }
  def closeVar(name:LocalName) = {
    sstate.foreach(s => component.foreach(c =>
      s.addVarNotation(name,id,opprec,argprecs,c,opcomponent)
    ))
  }
}
trait SHTMLONotationComponent extends SHTMLObject {
  def close = {
    findAncestor{case not : SHTMLONotation => not}.foreach(_.component = Some(this))
  }
}
trait SHTMLOOpNotationComponent extends SHTMLObject {
  def close = {
    findAncestor { case not: SHTMLONotation => not }.foreach(_.opcomponent = Some(this))
  }
}

trait SHTMLOMathStructure extends SHTMLObject with ModuleLike {
  val mp : MPath
  var parent : Option[MPath] = None
  var const : Option[Constant] = None
  var module : Option[NestedModule] = None
  val macroname: String
  lazy val modulename = LocalName.parse(mp.name + "-module")
  lazy val constantname = LocalName.parse(mp.name.tail.mkString("/"))
  def open: Unit = sstate.foreach { state =>
    val plang = findAncestor { case th: ModuleLike if th.language_theory.isDefined => th.language_theory.get }
    findAncestor { case th: ModuleLike if th.signature_theory.isDefined => th.signature_theory.get }.foreach { t =>
      parent = Some(t.path)
      val th = Theory(mp.parent, modulename, None)
      contentelem = Some(th.path)
      val nt = new NestedModule(t.toTerm, modulename.tail, th)
      module = Some(nt)
      doSourceRef(th)
      doSourceRef(nt)
      state.add(nt)
      if (constantname.toString.startsWith("EXTSTRUCT_")) {
        // TODO
      } else {
        val c = Constant(t.toTerm, constantname, Nil, Some(OMS(ModelsOf.tp)), Some(ModelsOf(th.path)), None)
        SHTMLContentManagement.addMacroName(macroname,c)
        SHTMLContentManagement.setStructureSymbol(nt,c.path)
        SHTMLContentManagement.setStructureModule(c,th.path)
        //c.setOrigin(GeneratedFrom(nt.path,this))
        // TODO elaborations
        doSourceRef(c)
        state.add(c)
        const = Some(c)
      }
      signature_theory = Some(th)
      plang.foreach { t =>
        //nt.setOrigin(GeneratedFrom(mp.parent ? modulename,this))
        val incl = PlainInclude(mp.parent ? modulename, t.path)
        //incl.setOrigin(GeneratedFrom(mp.parent ? modulename,this))
        state.add(incl)
        state.endAdd(incl)
      }
      state.rel(ULO.contains(RDFImplicits.pathToIri(t.path), RDFImplicits.pathToIri(th.path)))
      state.rel(ULO.mathstructure(RDFImplicits.pathToIri(th.path)))
      const.foreach(c => state.rel(ULO.mathstructure_of(RDFImplicits.pathToIri(c.path), RDFImplicits.pathToIri(th.path))))
    }
  }

  def close: Unit = {
    sstate.foreach { state =>
      signature_theory.foreach { th => parent.foreach {parent =>
        state.endAdd(th)
        const.foreach(state.check)
        th.getConstants.foreach{oc =>
          val c = Constant(OMMOD(parent),constantname / oc.name,Nil,None,
            None,None
          )
          // TODO definiens
          c.setOrigin(ElaborationOf(oc.path))
          doSourceRef(c)
          state.add(c)
        }
      }}
      language_theory.foreach { th =>
        state.endAdd(th)
      }
    }

  }
}

trait SHTMLORule extends SHTMLObject with IsTermWithArgs {
  def path : MPath

  override val notationid: String = ""

  override def headsymbol: Option[Term] = Some(OMAorAny(OMMOD(path),getArgs.map(_._2)))
  def close: Unit = {
    sstate.foreach{state =>
      findAncestor{case m: ModuleLike => m}.foreach{_.signature_theory.foreach{ mod =>
        state.addRule(mod.path,headsymbol.get)
      }}
    }
  }
}

trait SHTMLStatement extends SHTMLObject with SymbolLike with SHTMLGroupLike {
  var fors : List[GlobalName] = Nil
  val kind: String
  var id = ""
}

trait SHTMLMorphism extends SHTMLObject with SHTMLGroupLike {
  val istotal : Boolean
  val domain: MPath
  val path: GlobalName
  var structure : Option[Structure] = None

  def open: Unit = sstate.foreach { state =>
    findAncestor { case t: ModuleLike => t }.foreach { t =>
      t.signature_theory.foreach { th =>
        val struct = Structure(th.toTerm, path.name, OMMOD(domain), path.name match {
          case LocalName(ComplexStep(`domain`) :: Nil) => true
          case _ => false
        }, istotal)
        doSourceRef(struct)
        state.add(struct)
        structure = Some(struct)
      }
    }
  }
  def close(): Unit = sstate.foreach { state =>
    structure.foreach {struct =>
      state.endAdd(struct)
      state.check(struct)
    }
  }
}