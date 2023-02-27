package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMMOD, OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule, PlainInclude, RuleConstant, RuleConstantInterpreter, Structure}
import info.kwarc.mmt.api.{ComplexStep, ContainerElement, DPath, ElaborationOf, GeneratedFrom, GetError, GlobalName, LocalName, MPath, ParametricRule, Path, Rule, RuleSet, StructuralElement}
import info.kwarc.mmt.stex.Extensions.LateBinding
import info.kwarc.mmt.stex.{SCtx, SHTML, SHTMLHoas, STeXServer, STerm}
import info.kwarc.mmt.stex.rules.{BinRRule, BindingRule, ConjRule, ModelsOf, PreEqualRule, PreRule, Reorder, RulerRule, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.collection.mutable

trait SHTMLObject {
  type SHTMLClass <: SHTMLObject

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
  def getRuleContext : Context = findAncestor {
    case hrc:HasRuleContext => hrc.context
  }.getOrElse(Context.empty)

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
    server.getRuler(tm,self.getRuleContext ++ self.getVariableContext)
  }
  def update(se: StructuralElement)
  def error(s:String): Unit
  def error(e:info.kwarc.mmt.api.Error) : Unit
  def doc : Document
  def applyTerm(tm : Term)(implicit self:SHTMLClass): Term
  def applyTopLevelTerm(tm : Term)(implicit self:SHTMLClass): Term
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
  def addSymdoc(fors : List[GlobalName],id:String,html:scala.xml.Node,language:String)(implicit context : SHTMLClass) : Unit
  def addExample(fors : List[GlobalName],id:String,html:scala.xml.Node)(implicit context : SHTMLClass) : Unit
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
  var doc : Option[Document] = None
  def mp = path ? LocalName(language)

  sstate.foreach { s =>
    if (path == s.doc.path) {
      doc = Some(s.doc)
      s.add(s.doc)
    } else {
      val d = new Document(path)
      doc = Some(d)
      s.add(d)
    }
  }

  def open: Unit = sstate.foreach { state =>
    if (language.isEmpty) language = "en"
    //if (language != "") {
    val lang = Theory(path, LocalName(language), None)
    state.server.addLanguage(language, lang)
    doSourceRef(lang)
    language_theory = Some(lang)
    state.add(lang)
    doc.foreach{d =>
      state.add(MRef(d.path,lang.path))
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
    val pth = findAncestor { case th: ModuleLike if th.signature_theory.isDefined => th }
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
        t.signature_theory.foreach { t =>
          val th = Theory(mp.parent, mp.name, metatheory) // TODO parameters
          doSourceRef(th)
          val nt = new NestedModule(t.toTerm, mp.name.tail, th)
          state.add(nt)
          signature_theory = Some(th)
        }
        if (language != "") {
          t.language_theory.foreach { t =>
            val th = Theory(mp.parent / mp.name, LocalName(language), metatheory) // TODO parameters
            doSourceRef(th)
            val nt = new NestedModule(t.toTerm, LocalName(language), th)
            doSourceRef(th)
            state.add(nt)
            signature_theory = Some(th)
          }
        }
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(mp.parent, mp.name, metatheory) // TODO parameters
      doSourceRef(th)
      signature_theory = Some(th)
      state.add(th)
      if (language.isEmpty) language = "en"
      //if (language != "") {
      val lang = Theory(mp.parent / mp.name, LocalName(language), metatheory)
      state.server.addLanguage(language,lang)
      doSourceRef(lang)
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(mp, lang.path)
      //incl.setOrigin(GeneratedFrom(lang.path,this))
      state.add(incl)
      state.endAdd(incl)
      state.check(incl)
      _context = _context ++ Context(lang.path)
      //}
    } else if (language == signature) {
      val sig = Theory(mp.parent, mp.name, metatheory) // TODO parameters
      doSourceRef(sig)
      signature_theory = Some(sig)
      state.add(sig)
      val lang = Theory(mp.parent / mp.name, LocalName(language), metatheory)
      state.server.addLanguage(language,lang)
      doSourceRef(lang)
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(sig.path, lang.path)
      //incl.setOrigin(GeneratedFrom(lang.path, this))
      state.add(incl)
      state.endAdd(incl)
      state.check(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(mp.parent / mp.name, LocalName(language), metatheory)
      state.server.addLanguage(language, lang)
      doSourceRef(lang)
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(mp, lang.path)
      //incl.setOrigin(GeneratedFrom(lang.path, this))
      state.add(incl)
      state.endAdd(incl)
      state.check(incl)
      _context = _context ++ Context(lang.path)
    }
    (signature_theory.toList ::: language_theory.toList).foreach { t =>
      findAncestor { case th: HasRuleContext => th } match {
        case Some(st: SHTMLDocument) =>
          state.add(MRef(st.path, t.path.toMPath))
        case _ =>
          state.add(MRef(state.doc.path, t.path.toMPath))
      }
      state.check(t)
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
      findAncestor { case ml: ModuleLike => ml }.foreach { ml =>
        ml.language_theory.foreach { th =>
          val pl = PlainInclude(mp, th.path)
          doSourceRef(pl)
          state.add(pl)
          state.endAdd(pl)
          state.check(pl)
        }
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
  def close: Unit = {
    sstate.foreach { state =>
      val itp = getType.map(state.applyTopLevelTerm)
      val idf = defi.map(state.applyTopLevelTerm)
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
      if (macroname.nonEmpty) state.server.addMacroName(macroname,c)
      if (args.nonEmpty) state.server.addArity(args,c)
      if (assoctype.nonEmpty) state.server.addAssoctype(assoctype, c)
      if (reorderargs.nonEmpty) state.server.addReorder(reorderargs, c)
      doSourceRef(c)
      state.add(c)
      state.check(c)
      /*if (reorderargs.nonEmpty) addRule(c, "reorder", Reorder, List(StringLiterals(reorderargs)))
      assoctype match {
        case "" =>
        case "pre" =>
          addRule(c, "pre", PreRule, List(c.toTerm))
        case "conj" =>
          addRule(c, "conj", ConjRule, List(c.toTerm))
        case "bin"|"binr" =>
          addRule(c, "binr", BinRRule, List(c.toTerm))
        case _ =>
          print("TODO")
      } */
    }
  }
}
trait SHTMLOVarDecl extends SymbolLike with HasMacroName {
  val name : LocalName
  var bind = false

  def closeSeq = {
    sstate.foreach { state =>
      val tp = getType match {
        case Some(OMV(x)) => getVariableContext.findLast(_.name == x) match {
          case Some(vd) if vd.metadata.get(SHTML.flatseq.sym).contains(OMS(SHTML.flatseq.sym)) => Some(OMV(x))
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
            val cname = newname(t, name)
            val c = Constant(t.toTerm, cname, Nil, tp, df, Some(("variable" :: rl.toList).mkString(" ")) )
            hoas.foreach(_.apply(c))
            if (macroname.nonEmpty) state.server.addMacroName(macroname, c)
            if (args.nonEmpty) state.server.addArity(args, c)
            if (assoctype.nonEmpty) state.server.addAssoctype(assoctype, c)
            if (reorderargs.nonEmpty) state.server.addReorder(reorderargs, c)
            vd.metadata.update(SHTML.headterm, c.toTerm)
            doSourceRef(c)
            state.add(c)
            //state.check(c)
          case None =>
            hoas.foreach(_.apply(vd))
            if (macroname.nonEmpty) state.server.addMacroName(macroname, vd)
            if (args.nonEmpty) state.server.addArity(args, vd)
            if (assoctype.nonEmpty) state.server.addAssoctype(assoctype, vd)
            if (reorderargs.nonEmpty) state.server.addReorder(reorderargs, vd)
        }
        vd.metadata.update(SHTML.flatseq.sym, OMS(SHTML.flatseq.sym))
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
            val cname = newname(t, name)
            val c = Constant(t.toTerm, cname, Nil, tp, df, Some(("variable" :: rl.toList).mkString(" ")))
            hoas.foreach(_.apply(c))
            if (macroname.nonEmpty) state.server.addMacroName(macroname, c)
            if (args.nonEmpty) state.server.addArity(args, c)
            if (assoctype.nonEmpty) state.server.addAssoctype(assoctype, c)
            if (reorderargs.nonEmpty) state.server.addReorder(reorderargs, c)
            vd.metadata.update(SHTML.headterm, c.toTerm)
            doSourceRef(c)
            state.add(c)
            //state.check(c)
          case None =>
            hoas.foreach(_.apply(vd))
            if (macroname.nonEmpty) state.server.addMacroName(macroname, vd)
            if (args.nonEmpty) state.server.addArity(args, vd)
            if (assoctype.nonEmpty) state.server.addAssoctype(assoctype, vd)
            if (reorderargs.nonEmpty) state.server.addReorder(reorderargs, vd)
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
          case Some(vd) if vd.metadata.get(SHTML.flatseq.sym).nonEmpty =>
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
  def close = {
    sstate.flatMap { state =>
      findAncestor{case m : ModuleLike if m.language_theory.isDefined => m}.flatMap {mod =>
        getTerm match {
          case OMS(_)|OMV(_) => None
          case SHTMLHoas.OmaSpine(_,_,args) if args.forall(a => a.isInstanceOf[OMV] && state.isUnknown(a.asInstanceOf[OMV])) => None
          case o =>
            val df = state.applyTopLevelTerm(o)
            val c = Constant(OMMOD(mod.language_theory.get.path), state.termname, Nil, None, Some(df), Some("mmt_term"))
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
      headsymbol.foreach{h => ret.metadata.update(SHTML.headterm,h) }
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
      headsymbol.foreach { h => ret.metadata.update(SHTML.headterm, h) }
    }
    Some(ret)
  }
}
trait SHTMLOMIDorOMV extends IsTerm {
  override def getTermI : Option[Term] = {
    val ret = head
    doSourceRef(ret)
    if (!headsymbol.contains(head)) {
      headsymbol.foreach { h => ret.metadata.update(SHTML.headterm, h) }
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
    findAncestor { case th: ModuleLike => th }.foreach { ml =>
      parent = Some(ml.mp)
      ml.signature_theory.foreach { t =>
        val th = Theory(mp.parent, modulename, None)
        val nt = new NestedModule(t.toTerm, modulename.tail, th)
        module = Some(nt)
        doSourceRef(th)
        doSourceRef(nt)
        state.add(nt)
        if (constantname.toString.startsWith("EXTSTRUCT_")) {
          // TODO
        } else {
          val c = Constant(t.toTerm, constantname, Nil, Some(OMS(ModelsOf.tp)), Some(ModelsOf(th.path)), None)
          state.server.addMacroName(macroname,c)
          nt.metadata.update(ModelsOf.tp, c.toTerm)
          c.metadata.update(ModelsOf.sym, th.toTerm)
          //c.setOrigin(GeneratedFrom(nt.path,this))
          // TODO elaborations
          doSourceRef(c)
          state.add(c)
          const = Some(c)
        }
        signature_theory = Some(th)
      }
      ml.language_theory.foreach { t =>
        val th = Theory(t.parent, t.name / modulename.tail, None)
        val nt = new NestedModule(t.toTerm,modulename.tail, th)
        doSourceRef(th)
        doSourceRef(nt)
        //nt.setOrigin(GeneratedFrom(mp.parent ? modulename,this))
        val incl = PlainInclude(mp.parent ? modulename, th.path)
        //incl.setOrigin(GeneratedFrom(mp.parent ? modulename,this))
        state.add(nt)
        state.add(incl)
        state.endAdd(incl)
        language_theory = Some(th)
      }
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

  def open(): Unit = sstate.foreach { state =>
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

/*
case class HTMLCopyModule(orig : HTMLParser.HTMLNode,istotal:Boolean) extends OMDocHTML(orig) with HasDomain with HasComplexAssignments {
  override def copy: this.type = {
    val ret = HTMLCopyModule(orig.copy,istotal)
    ret.moduleassignments = moduleassignments
    ret.assignments = assignments
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  def path = Path.parseS(resource)
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor{
      case t : HTMLModuleLike => t
    }.foreach { t => t.signature_theory.foreach{th =>
      val struct = Structure(t.sighome.get,path.name,OMMOD(domain.get),false,istotal)
      sourceref.foreach(s => SourceRef.update(struct,s))
      state.add(struct)
      moduleassignments.foreach {
        case (mp,cp) =>
          val s = Include.assignment(struct.toTerm,mp,Some(cp match {
            case s : GlobalName => OMS(s)
            case m : MPath =>
              OMIDENT(OMMOD(m))
          }))
          state.add(s)
          state.endAdd(s)
      }
      assignments.reverse.foreach {
        case (p,alias,df) =>
          val npath = state.getO(p) match {
            case Some(d : Declaration) => d.path
            case _ => p
          }
          val c = Constant(struct.toTerm,ComplexStep(npath.module) / npath.name,alias.toList,None,df,None)
          state.getHOAS.foreach(OMDocHTML.setHOAS(c, _))
          df match {
            case Some(OMS(p)) =>
              state.getO(p) match {
                case Some(d) => d.metadata.getAll.foreach(c.metadata.update)
                case _ =>
                  print("")
              }
            case Some(o) =>
              println(o)
            case None =>
          }
          state.getO(p) match {
            case Some(d) => d.metadata.getAll.foreach(c.metadata.update)
            case _ =>
              print("")
          }
          state.add(c)
          state.check(c)
      }
      state.endAdd(struct)
      state.check(struct)

    }}
  }
}

case class HTMLRealization(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDomain with HasComplexAssignments {
  def path = Path.parseS(resource)
  override def copy: this.type = {
    val ret = HTMLRealization(orig.copy)
    ret.moduleassignments = moduleassignments
    ret.assignments = assignments
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor{
      case t : HTMLModuleLike => t
    }.foreach { t => t.signature_theory.foreach{th =>
      val struct = Structure(t.sighome.get,LocalName(domain.get),OMMOD(domain.get),true,true)
      sourceref.foreach(s => SourceRef.update(struct,s))
      state.add(struct)
      moduleassignments.foreach {
        case (mp,cp) =>
          val s = Include.assignment(struct.toTerm,mp,Some(cp match {
            case s : GlobalName => OMS(s)
            case m : MPath =>
              OMIDENT(OMMOD(m))
          }))
          state.add(s)
          state.endAdd(s)
      }
      assignments.reverse.foreach {
        case (p,alias,df) =>
          val npath = state.getO(p) match {
            case Some(d : Declaration) => d.path
            case _ => p
          }
          val c = Constant(struct.toTerm,ComplexStep(npath.module) / npath.name,alias.toList,None,df,None)
          state.getHOAS.foreach(OMDocHTML.setHOAS(c, _))
          state.add(c)
          state.check(c)
      }
      state.endAdd(struct)
      state.check(struct)

    }}
  }
}


class OMDocHTML(orig : HTMLParser.HTMLNode) extends CustomHTMLNode(orig) {

  def termReference = attributes.get((namespace,"data-stex-term-reference"))
  def setTermReference(p : GlobalName) = attributes((namespace,"data-stex-term-reference")) = p.toString

  override def copy : this.type = try {
    val ret = this.getClass.getConstructor(classOf[HTMLParser.HTMLNode]).newInstance(orig.copy).asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret
  } catch {
    case e : Throwable =>
      e.printStackTrace()
      print("")
      throw e
  }
  def resource = attributes.getOrElse((namespace,"resource"),"")
  def property = attributes.getOrElse((namespace,"property"),"")
  val sstate = state match {
    case s : SemanticState => Some(s)
    case _ => None
  }
  def getTerm : Option[Term] = {
    this match {
      case t : HTMLTerm => return Some(t.toTerm)
      case _ => children.foreach{case c : OMDocHTML =>
        c.getTerm match {
          case Some(t) => return Some(t)
          case _ =>
        }
      case _ =>
      }
    }
    None
  }

  def findTerm : Option[Term] = findTermI(this)
  private def findTermI(h : HTMLNode): Option[Term] = {
    h match {
      case o:HTMLTerm =>
        val t = o.toTerm
        h.sourceref.foreach(s => SourceRef.update(t,s))
        Some(t)
      case _ => h.children.foreach { c =>
        findTermI(c) match {
          case Some(t) => return Some(t)
          case _ =>
        }
      }
      None
    }
  }

  def forceTerm : Term = {
    getTerm match {
      case Some(t) => t
      case None => forceTerm2(this)
    }
  }
  protected def forceTerm2(n : HTMLNode): Term = n.children match {
    case Nil if n.isInstanceOf[HTMLText] =>
      try {STeX.informal(n.parent.get.node) } catch {
        case e: Throwable =>
          e.printStackTrace()
          ???
      }
    case List(_: HTMLText) | Nil =>
      try {STeX.informal(n.node) } catch {
        case e: Throwable =>
          e.printStackTrace()
          ???
      }
    case List(a : OMDocHTML) if n.isInstanceOf[MathMLNode] && n.label == "mrow" => a.forceTerm
    case List(a) if n.isInstanceOf[MathMLNode] && n.label == "mrow" =>forceTerm2(a)
    case _ =>
      val args = n.children.flatMap{
        case o : HTMLTerm => Some(o.toTerm)
        case _ : NotationComponent => None
        case o => Some(forceTerm2(o))
      }
      STeX.informal.op(n.label, args)
  }
}


class HTMLDocument(val path : DPath,orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasLanguage with HasRuleContext {
  val doc = new Document(path)
  def init = {
    sourceref.foreach(s => SourceRef.update(doc, s))
    sstate.foreach { state =>
      state.add(doc)
    }
  }
  init

  override def copy: HTMLDocument.this.type = {
    val self = this
    val ret = new HTMLDocument(path,orig.copy) {
      override val doc = self.doc
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = sstate.foreach { state =>
    state.endAdd(doc)
  }
}



case class HTMLIncludeproblem(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLDocument => t
      }.foreach { doc =>
        val dref = DRef(doc.doc.path, Path.parseD(resource + ".omdoc", NamespaceMap.empty))
        dref.setOrigin(GeneratedDRef)
        state.add(dref)
      }
    }
  }
  init

  override def copy: this.type = {
    val ret = new HTMLIncludeproblem(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

trait HasLanguage extends OMDocHTML {
  var language : String = ""
}

trait HasType extends OMDocHTML {
  var tp : Option[Term] = None
}

trait HasDefiniens extends OMDocHTML {
  var df : Option[Term] = None
}

trait HasDefinientia extends OMDocHTML {
  var dfs: mutable.HashMap[GlobalName,Term] = mutable.HashMap.empty
}

trait HasArity extends OMDocHTML {
  var arity: String = ""
}

trait HasAssocType extends OMDocHTML {
  var assoctype: String = ""
}

trait HasReorderArgs extends OMDocHTML {
  var reorders: String = ""
}

trait HasMacroName extends OMDocHTML {
  var macroname: String = ""
}

trait HasSimpleAssignments extends OMDocHTML {
  var assignments: List[(GlobalName,Term)] = Nil
}

trait HasComplexAssignments extends OMDocHTML {
  var assignments: List[(GlobalName,Option[LocalName],Option[Term])] = Nil
  var moduleassignments : List[(MPath,ContentPath)] = Nil
}

trait HasDomain extends OMDocHTML {
  var domain : Option[MPath] = None
}

case class HTMLDonotcopy(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDomain {
  def path = Path.parseMS(resource,NamespaceMap.empty)
  override def onAdd: Unit = sstate.foreach {state =>
    collectAncestor {
      case ha: HasComplexAssignments => ha
    }.foreach {ha => domain.foreach {dom =>
      ha.moduleassignments ::= (dom,path)
    }}
  }
}

trait HTMLConstant extends OMDocHTML {
  def path = Path.parseS(resource,NamespaceMap.empty)
  var role = ""
}

case class HTMLDomainComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init = {
    collectAncestor {
      case hl: HasDomain =>
        hl
    }.foreach { ha =>
      ha.domain = Some(Path.parseM(resource))
    }
  }
  init

  override def copy: this.type = {
    val ret = new HTMLDomainComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLComplexAssignment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDefiniens {
  var alias : String = ""
  def path = Path.parseS(resource)
  override def onAdd: Unit = {
    collectAncestor{
      case hl : HasComplexAssignments =>
        hl
    }.foreach { ha => sstate.foreach{ state =>
      (alias,df) match {
        case ("",None) =>
        case ("",Some(df)) => ha.assignments ::= (path,None,Some(state.applyTopLevelTerm(df)))
        case (s,Some(df)) => ha.assignments ::= (path,Some(LocalName(s)),Some(state.applyTopLevelTerm(df)))
        case (s,None) => ha.assignments ::= (path,Some(LocalName(s)),None)
      }
    }}
  }
}

case class HTMLAliasComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case ass : HTMLComplexAssignment => ass
    }.foreach(_.alias = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLAliasComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLSimpleAssignment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def copy: this.type = {
    val ret = new HTMLSimpleAssignment(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  def init: Unit = {
    collectAncestor {
      case hl: HasSimpleAssignments =>
        hl
    }.foreach { ha =>
      val (dm, tg) = {
        val ls = resource.split(',')
        assert(ls.length == 2)
        (Path.parseS(ls.head.split("#").head), {
          if (ls(1).startsWith("var://")) {
            val rest = ls(1).drop(6).split("#").head
            if (rest.contains('.')) {
              val (v, field) = {
                val ls = rest.split('.')
                (ls.head, ls(1))
              }
              Getfield(OMV(v), LocalName(field))
            } else OMV(rest)
          } else if (ls(1).startsWith("varseq://")) {
            val rest = resource.drop(9).split("#").head
            val ret = OMV(rest)
            ret.metadata.update(STeX.flatseq.sym, OMS(STeX.flatseq.sym))
            ret
          } else OMID(Path.parseS(ls(1).split("#").head))
        })
      }
      ha.assignments ::= (dm, tg)
    }
  }
  init
}

case class HTMLLanguageComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case hl: HasLanguage =>
        hl
    }.foreach(_.language = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLLanguageComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLSignatureComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case th: HTMLTheory => th
    }.foreach(_.signature = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLSignatureComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLArityComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case ha: HasArity => ha
    }.foreach(_.arity = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLArityComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLMacroNameComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case ha: HasMacroName => ha
    }.foreach(_.macroname = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLMacroNameComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLMetatheoryComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    if (resource.nonEmpty) {
      collectAncestor {
        case ha: HTMLModuleLike => ha
      }.foreach(_.metatheory = Some(Path.parseM(resource)))
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLMetatheoryComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLBindTypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def copy: this.type = {
    val ret = new HTMLBindTypeComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  def init: Unit = {
    collectAncestor {
      case g: HTMLGroupLike => g
    }.foreach { g =>
      val (tp, v) = {
        val ls = resource.split(',')
        (ls.head, ls(1))
      }
      g.getVariables.findLast(_.name.toString == v) match {
        case Some(vd) =>
          val nvd = VarDecl(vd.name, vd.feature, vd.tp, vd.df, vd.not)
          vd.metadata.getAll.foreach(nvd.metadata.update)
          nvd.metadata.update(STeX.meta_quantification, OMS(
            if (tp == "forall") STeX.meta_qforall else STeX.meta_qexists
          ))
          g.variables = g.variables ++ nvd
        case _ =>
          print("")
      }
    }
  }
  init
}

case class HTMLAssoctypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case ha: HasAssocType => ha
    }.foreach(_.assoctype = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLAssoctypeComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLReorderComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init: Unit = {
    collectAncestor {
      case ha: HasReorderArgs => ha
    }.foreach(_.reorders = resource)
  }
  init
  override def copy: this.type = {
    val ret = new HTMLReorderComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}

case class HTMLTypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init: Unit = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLTypeComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }


  override def onAdd: Unit = {
    sstate.foreach { state =>
      state.in_term = in_term
      collectAncestor {
        case hd: HasType => hd
      }.foreach(_.tp = findTerm)
    }
  }
}

case class HTMLDefComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init: Unit = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLDefComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }

  override def onAdd: Unit = {
    sstate.foreach { state =>
      state.in_term = in_term
      collectAncestor {
        case hd: HasDefiniens =>
          hd.df = findTerm
          true
        case hd : HasDefinientia =>
          resource.split(',').foreach {r =>
            if (r.nonEmpty) {
              val gn = Path.parseS(r)
              findTerm.foreach(hd.dfs(gn) = _)
              collectAncestor { case s: HTMLStatement => s}.foreach(_.fors ::= gn)
            }
          }
          true
      }
    }
  }
}

case class HTMLArgMarker(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}

case class HTMLNotationFragment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = if (resource != "") collectAncestor {
    case s: NotationLike => s
  }.foreach(_.fragment = resource)
}
case class HTMLNotationPrec(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = if (resource != "") collectAncestor {
    case s: NotationLike => s
  }.foreach(_.precedence = resource)
}

case class HTMLNotationComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init: Unit = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLNotationComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  def getNotChild() : Option[HTMLTerm] = {
    iterate{
      case t : HTMLTerm => return Some(t)
      case  _ =>
    }
    None
  }
  override def onAdd: Unit = {
    sstate.foreach(_.in_term = in_term)
    collectAncestor {
      case c : NotationLike =>
        c.notation = getNotChild()
    }
  }
}

case class HTMLNotationOpComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init: Unit = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLNotationOpComponent(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  def getNotChild() : Option[HTMLTerm] = {
    this.iterate{
      case t : HTMLTerm => return Some(t)
      case  _ =>
    }
    None
  }
  override def onAdd: Unit = {
    sstate.foreach(_.in_term = in_term)
    collectAncestor {
      case c : NotationLike =>
        c.opnotation = this.getNotChild()
    }
  }
}

trait HTMLModuleLike extends OMDocHTML with HasRuleContext with HasLanguage {
  var metatheory : Option[MPath] = None

  def path = Path.parseMS(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.toMPath.parent

  var signature_theory : Option[AbstractTheory] = None
  var language_theory : Option[AbstractTheory] = None
  def sighome = signature_theory match {
    case Some(t : Theory) => Some(t.toTerm)
    case Some(d : DerivedDeclaration) => Some(OMMOD(d.modulePath))
    case _ => None
  }

  def open: Unit
}

case class HTMLStructuralFeature(orig:HTMLParser.HTMLNode,feature:String) extends OMDocHTML(orig) with HTMLModuleLike {
  override def copy: this.type = {
    val ret = HTMLStructuralFeature(orig.copy,feature)
    ret._context = _context
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def open: Unit = sstate.foreach { state => collectAncestor {case th : HTMLModuleLike => th}.foreach {ml => ml.signature_theory.foreach {th =>
    val p = path
    val dd = new DerivedDeclaration(th.toTerm,p.name,feature,TermContainer.empty(),NotationContainer.empty())
    sourceref.foreach(s => SourceRef.update(dd,s))
    state.add(dd)
    _context = _context ++ Context(dd.modulePath)
    signature_theory = Some(dd)
    ml.language_theory.foreach{lt =>
      state.add(PlainInclude(dd.modulePath,lt.path.toMPath))
      language_theory = Some(lt)
    }
  }}}

  override def onAdd: Unit = {
    sstate.foreach { state => signature_theory.foreach{th =>
      state.endAdd(th)
      state.check(th)
    }}

  }
}

case class HTMLStructureFeature(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  private var parentmodule : Option[AbstractTheory] = None
  override def copy: this.type = {
    val ret = new HTMLStructureFeature(orig.copy)
    ret.parentmodule = parentmodule
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    ret.language = language
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def open: Unit = sstate.foreach { state => collectAncestor {case th : HTMLModuleLike => th}.foreach {ml => ml.signature_theory.foreach {t =>
    val th = Theory(path.module.parent,path.toMPath.name,None)
    parentmodule = Some(t)
    val nt = new NestedModule(t.toTerm,path.name,th)
    sourceref.foreach(s => SourceRef.update(th,s))
    sourceref.foreach(s => SourceRef.update(nt,s))
    state.add(nt)
    signature_theory = Some(th)
  }
    ml.language_theory.foreach {t =>
      val th = Theory(t.modulePath.parent,t.name / path.name,None)
      val nt = new NestedModule(t.toTerm,path.name,th)
      sourceref.foreach(s => SourceRef.update(th,s))
      sourceref.foreach(s => SourceRef.update(nt,s))
      val incl = PlainInclude(path.module.parent ? path.toMPath.name,th.path)
      state.add(nt)
      state.add(incl)
      state.endAdd(incl)
      language_theory = Some(th)
      this.language = ml.language
    }
  }}

  override def onAdd: Unit = {
    sstate.foreach { state =>
      signature_theory.foreach { th =>
        state.endAdd(th)
        state.check(th)
        val rname = LocalName(path.name.toString.dropRight(10))
        val c = Constant(parentmodule.get.toTerm, rname, Nil, Some(OMS(ModelsOf.tp)), Some(ModelsOf(path.toMPath)), None)
        sourceref.foreach(s => SourceRef.update(c,s))
        state.add(c)
      }
      language_theory.foreach { th =>
        state.endAdd(th)
        //state.check(th)
      }
    }

  }
}

case class HTMLTheory(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  var signature : String = ""

  _context = _context ++ Context(path.toMPath)

  override def copy: this.type = {
    val ret = new HTMLTheory(orig.copy)
    ret._context = _context
    ret.signature = signature
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    ret.metatheory = metatheory
    ret.language = language
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }

  def open : Unit = sstate.foreach { state =>
    val pth = collectAncestor {case th : HTMLTheory => th}
    metatheory.foreach { p =>
      state.getO(p) match {
        case None =>
          state.missings ::= p
          metatheory = None
        case _ =>
      }
    }
    pth match {
      case Some(t) =>
        t.signature_theory.foreach {t =>
          val th = Theory(dpath, path.name, metatheory) // TODO parameters
          sourceref.foreach(s => SourceRef.update(th,s))
          val nt = new NestedModule(t.toTerm,path.name.tail,th)
          state.add(nt)
          signature_theory = Some(th)
        }
        if (language != "") {
          t.language_theory.foreach { t =>
            val th = Theory(dpath / path.name, LocalName(language), metatheory) // TODO parameters
            sourceref.foreach(s => SourceRef.update(th,s))
            val nt = new NestedModule(t.toTerm,  LocalName(language), th)
            sourceref.foreach(s => SourceRef.update(nt,s))
            state.add(nt)
            signature_theory = Some(th)
          }
        }
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.add(th)
      if (language.isEmpty) language = "en"
      //if (language != "") {
        val lang = Theory(dpath / path.name,LocalName(language),metatheory)
        lang.metadata.update(STeX.meta_language,StringLiterals(language))
        sourceref.foreach(s => SourceRef.update(lang,s))
        language_theory = Some(lang)
        state.add(lang)
        val incl = PlainInclude(path.toMPath,lang.path)
        state.add(incl)
        state.endAdd(incl)
        _context = _context ++ Context(lang.path)
      //}
    } else if (language == signature) {
      val sig = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(sig,s))
      signature_theory = Some(sig)
      state.add(sig)
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(sig.path,lang.path)
      state.add(incl)
      state.endAdd(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(path.toMPath,lang.path)
      state.add(incl)
      state.endAdd(incl)
      _context = _context ++ Context(lang.path)
    }
    print("")
    (signature_theory.toList ::: language_theory.toList).foreach {t =>
      collectAncestor{ case th:HasRuleContext => th} match {
        case Some(st : HTMLDocument) => state.add(MRef(st.path,t.path.toMPath))
        case _ =>
          state.add(MRef(state.doc.path,t.path.toMPath))
      }
    }
  }

  var onEnd : List[Unit => Unit] = Nil

  override def onAdd = sstate.foreach { state =>
    onEnd.reverse.foreach(f => f(()))
    signature_theory.foreach(state.endAdd)
    language_theory.foreach(state.endAdd)
  }
}

case class HTMLProblem(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  var signature : String = ""

  override def copy: this.type = {
    val ret = new HTMLProblem(orig.copy)
    ret._context = _context
    ret.signature = signature
    ret.signature_theory = signature_theory
    ret.language_theory = language_theory
    ret.metatheory = metatheory
    ret.language = language
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }

  _context = _context ++ Context(path.toMPath)

  def open : Unit = sstate.foreach { state =>
    collectAncestor {case th : HTMLTheory => th} match {
      case Some(t) =>
        t.signature_theory.foreach {t =>
          val th = Theory(dpath, path.name, metatheory) // TODO parameters
          sourceref.foreach(s => SourceRef.update(th,s))
          val nt = new NestedModule(t.toTerm,path.name,th)
          state.add(nt)
          signature_theory = Some(th)
        }
        if (language != "") {
          t.language_theory.foreach { t =>
            val th = Theory(dpath / path.name, LocalName(language), metatheory) // TODO parameters
            val nt = new NestedModule(t.toTerm,  LocalName(language), th)
            sourceref.foreach(s => SourceRef.update(th,s))
            sourceref.foreach(s => SourceRef.update(nt,s))
            state.add(nt)
            signature_theory = Some(th)
          }
        }
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.add(th)
      if (language.isEmpty) language = "en"
      //if (language != "") {
        val lang = Theory(dpath / path.name,LocalName(language),metatheory)
        lang.metadata.update(STeX.meta_language,StringLiterals(language))
        sourceref.foreach(s => SourceRef.update(lang,s))
        language_theory = Some(lang)
        state.add(lang)
        val incl = PlainInclude(path.toMPath,lang.path)
        state.add(incl)
        state.endAdd(incl)
        _context = _context ++ Context(lang.path)
      //}
    } else if (language == signature) {
      val sig = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(sig,s))
      signature_theory = Some(sig)
      state.add(sig)
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(sig.path,lang.path)
      state.add(incl)
      state.endAdd(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.add(lang)
      val incl = PlainInclude(path.toMPath,lang.path)
      state.add(incl)
      state.endAdd(incl)
      _context = _context ++ Context(lang.path)
    }
    print("")
    (signature_theory.toList ::: language_theory.toList).foreach {t =>
      collectAncestor{ case th:HasRuleContext => th} match {
        case Some(st : HTMLDocument) => state.add(MRef(st.path,t.path.toMPath))
        case _ =>
          state.add(MRef(state.doc.path,t.path.toMPath))
      }
    }
  }

  override def onAdd = sstate.foreach { state =>
    signature_theory.foreach(state.endAdd)
    language_theory.foreach(state.endAdd)
  }
}


case class HTMLTheoryHeader(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = collectAncestor {
    case th : HTMLModuleLike => th
    /*case th : HTMLDerived =>
      th.open*/
  }.foreach(_.open)
}

case class HTMLSymbol(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLConstant with HasArity with HasMacroName with HasType with HasDefiniens with HasAssocType with HasReorderArgs {
  override def copy: this.type = {
    val ret = HTMLSymbol(orig.copy)
    ret.tp = tp
    ret.df = df
    ret.assoctype = assoctype
    ret.reorders = reorders
    ret.macroname = macroname
    ret.arity = arity
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case th: HTMLModuleLike => th
    }.foreach {th =>
        if (th.signature_theory.isDefined) {
          val c = Constant(th.sighome.get,path.name,Nil,tp.map(state.applyTopLevelTerm),df.map(state.applyTopLevelTerm),Some("stexsymbol"))
          OMDocHTML.setAssoctype(c,assoctype)
          OMDocHTML.setReorder(c,reorders)
          OMDocHTML.setMacroName(c,macroname)
          OMDocHTML.setArity(c,arity)
          state.getHOAS.foreach(OMDocHTML.setHOAS(c,_))
          sourceref.foreach(s => SourceRef.update(c,s))
          state.add(c)
          (df,tp) match {
            case (Some(OMS(_)),None) =>
            case _ =>
              state.check(c)
          }
          /*th match {
            case _:HTMLTheory =>
              state.check(c)
            case _ =>
          }*/
          assoctype match {
            case "" =>
            case "binr" | "bin" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocBinR.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.add(rc)
            case "binl" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocBinL.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.add(rc)
            case "conj" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocConj.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.add(rc)
            case "pre" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocPre.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.add(rc)
            case "pwconj" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocPwconj.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.add(rc)
            case _ =>
              ???
          }
        }
    }
  }
}

trait NotationLike extends OMDocHTML with HasArity {
  var fragment : String = ""
  var precedence : String = ""
  var notation : Option[HTMLNode] = None
  var opnotation : Option[HTMLNode] = None
}

case class HTMLNotation(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLConstant with NotationLike {
  override def copy: this.type = {
    val ret = HTMLNotation(orig.copy)
    ret.notation = notation.map(_.copy)
    ret.fragment = fragment
    ret.precedence = precedence
    ret.opnotation = opnotation.map(_.copy)
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach { t =>
        notation.foreach { not =>
          val th = t.signature_theory match {
            case Some(th) => Some(th)
            case _ => t.language_theory
          }
          th.foreach { th =>
            val tp = STeX.notation.tp(path, arity)
            val df = STeX.notation(not.node,precedence,fragment,opnotation.map(_.node))
            val c = Constant(t.sighome.getOrElse(th.toTerm),state.newName("notation"),Nil,Some(tp),Some(df),Some("notation"))
            sourceref.foreach(s => SourceRef.update(c,s))
            state.add(c)
          }
        }
    }
  }
}

trait HTMLVariable extends OMDocHTML with HasType {
  //var bindtype : String = ""
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        sourceref.foreach(s => SourceRef.update(vd,s))
        /*if (bindtype != "") {
          vd.metadata.update(STeX.meta_quantification,OMS(
            if (bindtype == "forall") STeX.meta_qforall else STeX.meta_qexists
          ))
        } */
        g.variables = g.variables ++ vd
      }
    }
  }
}

case class HTMLVarDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasMacroName with NotationLike with HasDefiniens with HasAssocType with HasReorderArgs {
  override def copy: this.type = {
    val ret = HTMLVarDecl(orig.copy)
    ret.tp = tp
    ret.df = df
    ret.arity = arity
    ret.assoctype = assoctype
    ret.macroname = macroname
    ret.notation = notation
    ret.opnotation = opnotation
    ret.precedence = precedence
    ret.fragment = fragment
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        sourceref.foreach(s => SourceRef.update(vd,s))
        state.getHOAS.foreach(OMDocHTML.setHOAS(vd, _))
        OMDocHTML.setArity(vd,arity)
        OMDocHTML.setAssoctype(vd,assoctype)
        OMDocHTML.setReorder(vd,reorders)
        OMDocHTML.setMacroName(vd,macroname)
        notation.foreach { not =>
          vd.metadata.update(STeX.notation.tp.sym, STeX.notation(
            not.node,precedence,fragment,opnotation.map(_.node)
          ))
        }
        g.variables = g.variables ++ vd
      }
    }
  }
}
case class HTMLVarSeqDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasMacroName with NotationLike {
  var startidx : Option[Term] = None
  var endidx : Option[Term] = None
  override def copy: this.type = {
    val ret = HTMLVarSeqDecl(orig.copy)
    ret.tp = tp
    ret.arity = arity
    ret.startidx = startidx
    ret.endidx = endidx
    ret.macroname = macroname
    ret.notation = notation
    ret.opnotation = opnotation
    ret.precedence = precedence
    ret.fragment = fragment
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        assert(startidx.isDefined && endidx.isDefined)
        tp = tp.map {
          case OMV(n) => state.getVariableContext.findLast(_.name == n).flatMap(_.tp) match {
            case Some(STeX.flatseq.tp(_)) => OMV(n)
            case _ => STeX.flatseq.tp(OMV(n))
          }
          case t => STeX.flatseq.tp(t)
        }
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        sourceref.foreach(s => SourceRef.update(vd,s))
        OMDocHTML.setArity(vd,arity)
        //OMDocHTML.setAssoctype(vd,assoctype)
        //OMDocHTML.setReorder(vd,reorders)
        OMDocHTML.setMacroName(vd,macroname)
        notation.foreach { not =>
          vd.metadata.update(STeX.notation.tp.sym, STeX.notation(
            not.node,precedence,fragment,opnotation.map(_.node)
          ))
        }
        g.variables = g.variables ++ vd
      }
    }
  }
}
case class HTMLVarSeqStart(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable {
  private var in_term = false
  def init= {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init

  override def copy: this.type = {
    val ret = new HTMLVarSeqStart(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = {
    sstate.foreach(_.in_term = in_term)
    collectAncestor {
      case c : HTMLVarSeqDecl => c
    }.foreach {p => p.startidx = findTerm.map {
      case STeX.informal.op("mrow",List(t)) => t
      case t => t
    } }
  }
}

case class HTMLVarSeqEnd(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable {
  private var in_term = false
  def init = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init

  override def copy: this.type = {
    val ret = new HTMLVarSeqEnd(orig.copy) {
      override def init = {}
    }
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = {
    sstate.foreach(_.in_term = in_term)
    collectAncestor {
      case c : HTMLVarSeqDecl => c
    }.foreach {p => p.endidx = findTerm.map {
      case STeX.informal.op("mrow",List(t)) => t
      case t => t
    } }
  }
}

case class HTMLVarStructDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasSimpleAssignments with HasDomain {
  override def copy: this.type = {
    val ret = HTMLVarStructDecl(orig.copy)
    ret.tp = this.tp
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = {
    tp = if (assignments.isEmpty) domain.map(ModelsOf(_)) else {
      val omls = assignments.reverse.map(p => OML(p._1.name.filter(_.isInstanceOf[SimpleStep]),None,Some(p._2)))
      domain.map(d => RecMerge(ModelsOf(OMMOD(d)),RecType.make(omls:_*)))
    }
    super.onAdd
  }
}

case class HTMLImport(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def domain = Path.parseM(resource)
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach{t => t.signature_theory.foreach { th =>
      try {
        val incl = PlainInclude(domain, t.sighome.get.toMPath)
        sourceref.foreach(s => SourceRef.update(incl, s))
        state.add(incl)
        state.endAdd(incl)
        state.check(incl)
      } catch {
        case a : AddError =>
      }
    }}
  }
}

case class HTMLUseModule(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def domain = Path.parseM(resource)
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach{t =>
        t.language_theory.foreach { th =>
          val incl = PlainInclude(domain, th.path.toMPath)
          sourceref.foreach(s => SourceRef.update(incl,s))
          state.add(incl)
          state.endAdd(incl)
        }
    }
  }
}

trait NotationComponent extends OMDocHTML {
  override def getTerm: Option[Term] = None
}

case class HTMLComp(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent {
  this.classes ::= "symcomp"
}
case class HTMLVarComp(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent {
  this.classes ::= "varcomp"
}

case class HTMLDefiniendum(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent with HasHead {
  def path = Path.parseMS(resource,NamespaceMap.empty)
  this.classes ::= "definiendum"
  def toTerm = OMID(path)
  collectAncestor {
    case s : HTMLStatement => s
  }.foreach(_.fors ::= path)
}

case class HTMLArg(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def forceTerm: Term = children match {
    case List(o:OMDocHTML) => o.forceTerm
    case _ => super.forceTerm2(this)
  }
}

class HTMLTopLevelTerm(val orig : OMDocHTML) extends OMDocHTML(orig) with HTMLConstant {
  private var in_term = false
  def init = {
    sstate.foreach{s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  override def copy : this.type = {
    val ret = new HTMLTopLevelTerm(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }

  var constant : Option[Constant] = None

  init

  override def onAdd: Unit = {
    children.foreach(c => orig.add(c.copy))
    orig.onAdd
    sstate.foreach{state =>
      state.in_term = in_term
      collectAncestor {
        case t: HTMLModuleLike => t
      }.foreach { th =>
        orig.getTerm match {
          case None | Some(OMV(_)) | Some(OMS(_)) =>
          case Some(tm) =>
            val t = state.applyTopLevelTerm(tm)
            if (th.language_theory.isDefined) {
              val c = Constant(th.language_theory.get.toTerm,state.newName("term"),Nil,None,Some(t),Some("term"))
              this.attributes((HTMLParser.ns_mmt,"term")) = c.path.toString
              sourceref.foreach(s => SourceRef.update(c,s))
              state.add(c)
              th match {
                case t:HTMLTheory =>
                  t.onEnd ::= {_ => state.check(c)}
                case _ =>
              }
              state.getO(c.path).foreach{case c : Constant =>
                constant = Some(c)
                setTermReference(c.path)
              case _ => }
            }
        }
    }}
  }
}

trait HTMLStatement extends OMDocHTML with HTMLGroupLike with HasLanguage {
  var typestrings : List[String] = Nil
  var fors = resource.split(',').filter(_.nonEmpty).map(s => Path.parseMS(s.trim,NamespaceMap.empty)).toList
  var name : String = ""
  var path: Option[GlobalName] = None

  def addSymbolDoc(paths:List[ContentPath]): Unit = if (paths.nonEmpty) {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLModuleLike => t
      }.foreach{ t =>
          if (language == "") language = t.language
          t.language_theory.foreach{th =>
            val c = Constant(OMID(th.path),state.newName("symboldoc"),Nil,None,Some(STeX.symboldoc(paths.distinct,language,this)),Some("symboldoc"))
            sourceref.foreach(s => SourceRef.update(c,s))
            state.add(c)
          }
      }
    }
  }

  def bindvars[A <: StatementBinderRule](tm : Term,cls:Class[A]) = {
    val bvs = this.getVariables.filter(_.metadata.get(STeX.meta_quantification).exists(_.value == OMS(STeX.meta_qforall)))
    if (bvs.isEmpty) tm else sstate.get.getRules.get(cls).headOption.map{rl =>
      SOMB(OMS(rl.sym),SCtx(bvs),STerm(tm))
      //val ntm = if (bvs.isEmpty) tm else STeX.binder(bvs, tm)
      //sstate.get.applyTopLevelTerm(ntm)
    }.getOrElse(tm)
  }

  override def onAdd: Unit = {
    super.onAdd
    fors = fors.distinct
  }
}

case class HTMLTypeStringComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init = {
    collectAncestor {
      case s : HTMLStatement => s
    }.foreach(_.typestrings = resource.split(',').map(_.trim).toList)
  }
  init
  override def copy : this.type = {
    val ret = new HTMLTypeStringComponent(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }

}

case class HTMLFromComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}
case class HTMLToComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}

case class HTMLStatementNameComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def init = {
    collectAncestor {
      case s: HTMLStatement => s
    }.foreach(_.name = resource)
  }
  init
  override def copy : this.type = {
    val ret = new HTMLStatementNameComponent(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
}
case class HTMLPremise(orig: HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false

  def init = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy: this.type = {
    val ret = new HTMLPremise(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = sstate.foreach { state =>
    state.in_term = in_term
    collectAncestor {
      case s: HTMLSAssertion => s
    }.foreach{s =>
      val name = if (resource.nonEmpty) LocalName(resource) else state.newName("v")
      getTerm.foreach {tp =>
        val ntp = s.judgment match {
          case Some(judg) => SOMA(OMS(judg), tp)
          case _ => tp
        }
        val vd = VarDecl(name,ntp)
        vd.metadata.update(STeX.meta_quantification, OMS(STeX.meta_qforall))
        s.variables = s.variables ++ vd
      }
    }
  }
}
case class HTMLConclusionComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy : this.type = {
    val ret = new HTMLConclusionComponent(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = sstate.foreach{ state =>
    state.in_term = in_term
    collectAncestor {
      case s: HTMLSAssertion => s
    }.foreach(_.conc = getTerm)
  }
}

case class HTMLSolution(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  this.classes ::= "solution"
}

case class HTMLSDefinition(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement with HasDefinientia {
  override def copy: this.type = {
    val ret = HTMLSDefinition(orig.copy)
    ret.fors = fors
    ret.dfs = dfs
    ret.path = path
    ret.name = name
    ret.typestrings = typestrings
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = { sstate.foreach{ state =>
    super.onAdd
    state.Search.addDefi(this)
    addSymbolDoc(fors)
    if (this.dfs.nonEmpty) {
      dfs.foreach {
        case (path,tm) =>
          val defi = state.applyTopLevelTerm(bindvars(tm,classOf[DefiPremiseRule.Premise]))
          val orig = state.getO(path)
          (orig,collectAncestor { case s : HTMLModuleLike => s}) match {
            case (Some(c : Constant),Some(t)) if t.signature_theory.isDefined && c.parent == t.sighome.get.toMPath && c.df.isEmpty =>
              val nc = Constant(c.home,c.name,c.alias,c.tp,Some(defi),c.rl)
              nc.metadata = c.metadata
              sourceref.foreach(s => SourceRef.update(nc,s))
              this.path = Some(nc.path)
              state.update(nc)
              //state.add(nc)
              state.check(nc)
            case _ =>
              println("TODO: Equality rule")
          }
          print("")
      }
    }
  }}
}
case class HTMLSParagraph(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  override def copy: this.type = {
    val ret = HTMLSParagraph(orig.copy)
    ret.fors = fors
    ret.path = path
    ret.name = name
    ret.typestrings = typestrings
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = {
    super.onAdd
    if (typestrings.contains("symdoc")) {
      sstate.foreach(_.Search.addDefi(this))
      addSymbolDoc(fors)
    }
  }
}

case class HTMLDoctitle(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd = {
    super.onAdd
    collectAncestor {
      case d : HTMLDocument => d
    }.foreach { d =>
      sstate.foreach(_.title = Some(this))
      val nnode = this.plaincopy
      nnode.label = "span"
      nnode.classes = Nil
      nnode.attributes.clear()
      d.doc.metadata.update(STeX.meta_doctitle,OMFOREIGN(nnode.node))
    }
  }
}

case class HTMLJudgment(orig: HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = {
    collectAncestor {
      case ass : HTMLSAssertion => ass
    }.foreach {ass =>
      try { ass.judgment = Some(Path.parseS(resource))} catch {
        case t =>
      }
    }

  }
}

case class HTMLSAssertion(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  var conc : Option[Term] = None
  var judgment: Option[GlobalName] = None
  override def copy: this.type = {
    val ret = HTMLSAssertion(orig.copy)
    ret.fors = fors
    ret.conc = conc
    ret.path = path
    ret.name = name
    ret.judgment = judgment
    ret.typestrings = typestrings
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = {
    super.onAdd
    sstate.foreach{ state => if (name.nonEmpty || fors.nonEmpty) { collectAncestor {
    case m:HTMLModuleLike => m
  }.foreach { m => m.signature_theory.foreach {sig =>
    val tm = (conc,judgment) match {
      case (Some(t),Some(judg)) => state.applyTopLevelTerm(bindvars(SOMA(OMS(judg),t),classOf[AssPremiseRule.Premise]))
      case (Some(t),None) => state.applyTopLevelTerm(bindvars(t,classOf[AssPremiseRule.Premise]))
      case (_,Some(judg)) => state.applyTopLevelTerm(SOMA(OMS(judg),forceTerm2(this)))
      case _ => state.applyTopLevelTerm(forceTerm2(this))
    }
      if (name.nonEmpty) {
        val c = Constant(m.sighome.get, LocalName(name), Nil, Some(tm), None, Some("stexsymbol"))
        sourceref.foreach(s => SourceRef.update(c, s))
        addSymbolDoc(List(c.path))
        this.path = Some(c.path)
        state.add(c)
        state.check(c)
      } else {
        addSymbolDoc(fors)
        fors.head match {
          case gn:GlobalName if gn.module == sig.path =>
            state.getO(gn) match {
              case Some(c : Constant) if c.tp.isEmpty =>
                val nc = Constant(c.home,c.name,c.alias,Some(tm),c.df,c.rl)
                nc.metadata = c.metadata
                state.update(nc)
                state.check(nc)
              case _ =>
            }
          case _ =>
        }
      }
    state.Search.addAssertion(this)
  }}}}}
}
case class HTMLSExample(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  override def copy: this.type = {
    val ret = HTMLSExample(orig.copy)
    ret.fors = fors
    ret.path = path
    ret.name = name
    ret.typestrings = typestrings
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd = {
    super.onAdd
    sstate.foreach{ state => if (name.nonEmpty) { collectAncestor {
    case m:HTMLModuleLike => m
  }.foreach { m => m.signature_theory.foreach {_ =>
    val c = Constant(m.sighome.get,LocalName(name),Nil,None,None,Some("stexsymbol"))
    sourceref.foreach(s => SourceRef.update(c,s))
    state.add(c)
    this.path = Some(c.path)
    state.check(c)
    state.Search.addExample(this)
  }}}}}
}
trait ProofEnv extends OMDocHTML
trait HTMLProofFrame extends OMDocHTML {
  var expanded = true
  var forthm : Option[GlobalName] = None
  var yields : Option[Term] = None
  var justification : Option[Term] = None

  override def onAdd: Unit = {
    super.onAdd
    sstate.foreach{ state =>
      (forthm,this) match {
        case (Some(_),_) =>
        case (_,st:HTMLStatement) => forthm = st.fors.collectFirst{case gn : GlobalName => gn}
        case _ =>
      }
      (yields,forthm) match {
        case (None,Some(gn)) =>
          state.getO(gn) match {
            case Some(c : Constant) if c.df.isDefined => yields = c.df
            case _ =>
          }
        //case (Some(tm),Some(gn)) => // check equality ?
        case (Some(tm),_) => yields = Some(state.applyTopLevelTerm(tm))
        case _ =>
      }
    }
  }
}
case class HTMLSProof(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLProofFrame with HTMLStatement with ProofEnv {}
case class HTMLSProofstep(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLProofFrame {}
case class HTMLSProofyield(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  private var in_term = false
  def init = {
    sstate.foreach { s =>
      in_term = s.in_term
      s.in_term = true
    }
  }
  init
  override def copy : this.type = {
    val ret = new HTMLSProofyield(orig.copy) {
      override def init = {}
    }.asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret.asInstanceOf[this.type]
  }
  override def onAdd: Unit = {
    sstate.foreach { _.in_term = in_term}
    super.onAdd
    sstate.foreach {_ => collectAncestor { case frame : HTMLProofFrame => frame} match {
      case Some(frame) =>
        frame.yields match {
          case None => frame.yields = getTerm
          case _ =>
        }
      case _ =>
    }}
  }
}
case class HTMLSProofsketch(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSubproof(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLProofFrame with HTMLStatement with ProofEnv {}
case class HTMLSpfcase(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLProofFrame {}
case class HTMLSpfeq(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}

case class HTMLSProoftitle(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}
case class HTMLSProofbody(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ProofEnv {
  override def onAdd = {
    super.onAdd
    collectAncestor {
      case m: HTMLProofFrame => m
    }.foreach { f => if (resource.contains("false")) f.expanded = false }
  }
}

case class HTMLFrame(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  this.classes ::= "frame"

  override def onAdd: Unit = {
    val ch = this.children
    val inner = add(<div class="inner-frame"/>)
    ch.foreach(inner.add)
    super.onAdd
  }
}
// ---------------------------------------------------------------------------------------------------------------------

trait HTMLTerm extends OMDocHTML {
  def toTerm : Term
  override def isEmpty = false
}

trait HasHead extends HTMLTerm {
  def head = {
    Path.parseMS(resource.split("#").head,NamespaceMap.empty)
  }
  def headTerm : Term = {
    val ret = if (resource.startsWith("var://")) {
      val rest = resource.drop(6).split("#").head
      if (rest.contains('.')) {
        val (v,field) = {
          val ls = rest.split('.')
          (ls.head,ls(1))
        }
        Getfield(OMV(v),LocalName(field))
      } else OMV(rest)
    } else if (resource.startsWith("varseq://")) {
      val rest = resource.drop(9).split("#").head
      val ret = OMV(rest)
      ret.metadata.update(STeX.flatseq.sym,OMS(STeX.flatseq.sym))
      ret
    } else if (head.name.toString.startsWith("field:") && head.name.toString.contains(".")) {
      val npath = head.module ? LocalName(head.name.toString.drop(6).takeWhile(_ != '.'))
      Getfield(OMS(npath),LocalName(head.name.toString.split('.').last))
    } else OMID(head)
    sourceref.foreach(s => SourceRef.update(ret,s))
    ret
  }
  def fragment = resource.split("#").tail.mkString("#")
}

trait ComplexTerm extends HTMLTerm with HasHead {
  def sortArgs : List[(Char,Term)] = {
    if (fragment.startsWith("CUSTOM-")) {
      val tms = children.flatMap(getCustomArgs).sortBy(_._1)
      var ret : List[(Char,List[Term])] = Nil
      tms.foreach {
        case (_,b@('i'|'b'),t) => ret ::= (b,List(t))
        case (i,b@('a'|'B'),t) if ret.length < i => ret ::= (b,List(t))
        case (_,_,t) => ret = (ret.head._1,t :: ret.head._2) :: ret.tail
      }
      ret.map {
        case (b@('i'|'b'),List(t)) => (b,t)
        case (b,List(OMV(v))) => sstate.get.getVariableContext.find(_.name == v) match {
          case Some(vd) => vd.tp match {
            case Some(STeX.flatseq.tp(_)) =>
              (b, OMV(v))
            case _ =>
              (b, STeX.flatseq(OMV(v)))
          }
          case _ =>
            (b, STeX.flatseq(OMV(v)))
        }
        case (b,ls) => (b,STeX.flatseq(ls.reverse :_*))
      }.reverse
    } else {
      val args = children.flatMap(getargs).sortBy(_._1).distinctBy(_._1)
      args.map(t => (t._2, t._3))
    }
  }

  private def getCustomArgs(top: HTMLNode) : List[(Int,Char,Term)] = top match {
    case a : HTMLArg => List((a.resource.tail.toInt,a.resource.head,a.findTerm.getOrElse(forceTerm2(a))))
    case n => n.children.flatMap(getCustomArgs)
  }

  private def getargs(top : HTMLNode) : List[(Int,Char,Term)] = top match {
    case a : HTMLArg if a.resource.head == 'a' || a.resource.head == 'B' =>
      val ret = top.children.flatMap(getargs)
      if (ret.isEmpty) {
        val forced = a.forceTerm
        (forced,sstate) match {
          case (OMV(v),Some(state)) => state.getVariableContext.findLast(_.name == v) match {
            case Some(vd) => vd.tp match {
              case Some(STeX.flatseq.tp(_)) =>
                return List((a.resource.tail.toInt,a.resource.head,forced))
              case _ =>
            }
            case _ =>
          }
          case _ =>
        }
        List((a.resource.tail.toInt,a.resource.head,STeX.flatseq(a.forceTerm)))
      } else {
        val as = ret.filter(t => t._2 == a.resource.head && t._1.toString.startsWith(a.resource.tail))
        val os = ret.filterNot(as.contains)
        (a.resource.tail.toInt, a.resource.head, STeX.flatseq(as.map(_._3): _*)) :: os
      }
    case a : HTMLArg if a.resource.head == 'A' => List((a.resource.tail.tail.toInt,a.resource.tail.head,a.forceTerm))
    case a : HTMLArg => List((a.resource.tail.toInt,a.resource.head,a.forceTerm))
    case n => n.children.flatMap(getargs)
  }
}

case class HTMLOMBIND(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {

  override def toTerm = {
    val args = sortArgs.map {
      case (b@('b'|'B'),tm) =>
        SCtx(sstate.get.makeBinder(tm,b == 'B'))
      case (_,tm) => STerm(tm)
    }
    val tm = SOMB(headTerm,args:_*) //OMBINDC(OMID(head),ctx,args)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    sourceref.foreach(s => SourceRef.update(tm,s))
    tm
  }
}

case class HTMLOMID(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm with HasHead {
  override def toTerm: Term = {
    val tm = headTerm
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    sourceref.foreach(s => SourceRef.update(tm,s))
    tm
  }
}

case class HTMLOMV(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm with HasHead {
  override def toTerm: Term = {
    val tm = headTerm
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    sourceref.foreach(s => SourceRef.update(tm,s))
    tm
  }
}

case class HTMLOMA(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {
  override def toTerm: Term = {
    val tm = OMDocHTML.OMAorSOMA(headTerm,sortArgs.map(_._2))
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    sourceref.foreach(s => SourceRef.update(tm,s))
    tm
  }
}

case class MathMLNode(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm {
  override def toTerm: Term = forceTerm2(this)
}


case class HTMLMMTRule(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {
  private var in_term = false
  def toTerm = ???
  lazy val path = Path.parseM(resource)
  sstate.foreach { s =>
    in_term = s.in_term
    s.in_term = true
  }

  override def onAdd: Unit = sstate.foreach { state =>
    sstate.foreach(_.in_term = in_term)
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach { t =>
        t.signature_theory.foreach { _ =>
          val args = sortArgs match {
            case List((_,STeX.informal(_))) => Nil
            case ls => ls.map(_._2)
          }
          val rc = state.rci(t.sighome.get.toMPath,OMAorAny(OMID(path),args.map(state.applyTerm)),true)
          sourceref.foreach(s => SourceRef.update(rc,s))
          state.add(rc)
        }
    }
  }

}
 */