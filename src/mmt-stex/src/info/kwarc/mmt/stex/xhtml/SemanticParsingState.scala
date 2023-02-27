package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{AddError, ComplexStep, ContainerElement, DPath, ErrorHandler, GetError, GlobalName, LocalName, MMTTask, MPath, MutableRuleSet, Path, RuleSet, StructuralElement, utils}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, History, MMTStructureChecker, RelationHandler, Solver}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, NotFound}
import info.kwarc.mmt.api.notations.{HOAS, HOASNotation, NestedHOASNotation}
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMFOREIGN, OMPMOD, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.{ParseResult, SourceRef}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, RuleConstantInterpreter, Structure}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.stex.{SHTML, STeXError, STeXServer}
import info.kwarc.mmt.stex.lsp.STeXLSPErrorHandler
import info.kwarc.mmt.stex.rules.HTMLTermRule
//import info.kwarc.mmt.stex.rules.{BindingRule, ConjunctionLike, ConjunctionRule, Getfield, HTMLTermRule, ModelsOf, ModuleType, RecType, SubstRule}
import info.kwarc.mmt.stex.search.SearchDocument
//import info.kwarc.mmt.stex.{NestedHOAS, OMDocHTML, SCtx, SOMA, SOMB, SOMBArg, STeX, STeXError, STeXHOAS, STerm, SimpleHOAS}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class SemanticState(val server:STeXServer, rules : List[HTMLRule], eh : ErrorHandler, val dpath : DPath) extends HTMLParser.ParsingState(server.ctrl,rules) with SHTMLState[SHTMLNode] {
  var docs = List(new Document(dpath))
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

  def applyTopLevelTerm(tm: Term)(implicit self:SHTMLNode) : Term = {
    val vars = self.getVariableContext.filter(isBound)
      .reverse.distinctBy(_.name).reverse
    var ntm = vars.foldRight(tm)((vd,t) => SHTML.binder(vd,t))//SHTML.binder(vars,tm)
/*
    var impls = Context.empty
    self.getVariableContext.reverse.foreach { v =>
      if (!impls.exists(_.name == v.name) && (ntm.freeVars.contains(v.name) || impls.exists(_.tp.exists(_.freeVars.contains(v.name)))))
        impls ++= v
    }
    var nobinds : List[LocalName] = Nil
    impls = ntm.freeVars.foldLeft(impls.reverse)((ctx,n) => {
      if (!ctx.exists(_.name == n)) {
        val tpn = this.getUnknownTp
        nobinds ::= tpn
        ctx ::: VarDecl(n,OMV(tpn)) :: Nil
      } else ctx
    })

 */
    /*val impls = self.getVariableContext.filter{ v =>
      ntm.freeVars.contains(v.name)//vars.exists(_.tp.exists(_.freeVars.contains(v.name)))
    }.reverse.distinctBy(_.name).reverse*/

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
            case Some(vd) =>
              vd.tp.foreach(traverse)
              vd.df.foreach(traverse)
              implbinds ++= vd
            case None =>
              val tpn = getUnknownTp
              tosolves ++= VarDecl(tpn)
              val vd = VarDecl(x, OMV(tpn))
              implbinds ++= vd//OMV(tpn))
          }
          v
        case _ => Traverser(this,t)
      }
    }
    val trav2 = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMV(x) if !nobinds.contains(x) && tosolves.exists(_.name == x) =>
          Solver.makeUnknown(x,con.map(v => OMV(v.name)))
        case _ => Traverser(this,t)
      }
    }
    ntm = trav1(ntm,())
    tosolves = tosolves.distinct
    implbinds = implbinds.sortBy(vd => self.getVariableContext.indexOf(vd) match {case -1 => 10000 case i => i})
    ntm = trav2(SHTML.implicit_binder(implbinds,ntm),())
    if (tosolves.isEmpty) ntm else OMBIND(OMS(ParseResult.unknown),tosolves,ntm)
/*
    val implctx = mutable.Map.empty[LocalName, (Term,Option[Term])]

    val implicits = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State) = t match {
        case v@OMV(x) if (isUnknown(v) || !con.isDeclared(x)) =>
          implctx.getOrElseUpdate(x,
            (if (nobinds.contains(x)) v else
            Solver.makeUnknown(x,
              con.filterNot(_.name == x).map(v => OMV(v.name)) /*:::
                implctx.filterNot(_._1 == x).values.toList*/
            ),con.findLast(_.name == x).flatMap(_.tp)) //(con ++ implctx.keys.map(VarDecl(_))).map(v => OMV(v.name)).filterNot(_.name == x))
          )._1
        case _ => Traverser(this, t)
      }
    }

 */

    //val ntp = implicits(SHTML.implicit_binder(impls,ntm),())
    //if (implctx.isEmpty) ntp else OMBIND(OMS(ParseResult.unknown),implctx.map(p => VarDecl(p._1,None,p._2._2,None,None)).toList,ntp)
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

  private lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore, new MMTTask {})

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
      val body = maindoc.get()()("body").head
      doc.add("content", makeString(body), body.children.map(_.toString).mkString, doctitle.toList.flatMap { t =>
        val nt = t.copy
        nt.plain.attributes.remove((nt.namespace, "style"))
        List(("title", makeString(nt)), ("titlesource", nt.toString))
      }: _*)
      definitions.foreach(d => doc.add("definition", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.mkString(", ")) ::
          d._1.path.map(p => ("path", p.toString)).toList ::: (d._2 match {
          case Some(mp) => List(("module",mp.toString))
          case _ => Nil
        }): _*
      ))
      assertions.foreach(d => doc.add("assertion", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.mkString(", ")) ::
          d._1.path.map(p => ("path", p.toString)).toList ::: (d._2 match {
          case Some(mp) => List(("module", mp.toString))
          case _ => Nil
        }): _*
      ))
      examples.foreach(d => doc.add("example", makeString(d._1.copy), d._1.toString,
        ("for", d._1.fors.mkString(", ")) ::
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


  /*
  def doc = maindoc.doc
  var in_term = false



  private val names = mutable.HashMap.empty[String,Int]

  def newName(s : String) = {
    if (names.contains(s)) {
      names(s) = names(s) + 1
      LocalName(s + names(s).toString)
    } else {
      names(s) = 0
      LocalName(s)
    }
  }

  lazy val rci = new RuleConstantInterpreter(controller)

  def applyTopLevelTerm(tm : Term) = {
    val ntm = applyTerm(tm)
    class NameSet {
      var frees: List[VarDecl] = Nil
      var unknowns: List[LocalName] = Nil
    }
    val traverser = new Traverser[(NameSet,Boolean)] {

      object IsSeq {
        def apply(tm : Term) = tm match {
          case STeX.flatseq(_) => true
          case OMV(_) if tm.metadata.get(STeX.flatseq.sym).nonEmpty => true
          case _ => false
        }
        def unapply(tms : List[Term]) = {
          val i = tms.indexWhere(apply)
          if (i == -1) None else {
            Some(tms.take(i),tms(i),tms.drop(i+1))
          }
        }
      }

      def getRuleInfo(tm : Term) : Option[(List[Int],Option[String])] =
        OMDocHTML.getRuleInfo(tm)(controller,getVariableContext).map(t => (t._1,t._2))

      def reorder(tm: OMA)(implicit cont:Context, names: (NameSet,Boolean)): Term = {
        val SOMA(f, args) = tm
        val (ros, assoc) = getRuleInfo(f).getOrElse {
          return tm
        }
        val reordered = ros match {
          case Nil => args
          case ls => ls.map(args(_)) //OMA(tm.fun, ls.map(tm.args(_)))
        }
        assoc match {
          case Some("conj") =>
            reordered match {
              case IsSeq(pre, STeX.flatseq(a :: b :: Nil), Nil) =>
                val ret = SOMA(f, pre ::: List(a, b) :_*)
                ret.copyFrom(tm)
                ret
              case IsSeq(pre, STeX.flatseq(a :: Nil), List(b)) =>
                val ret = SOMA(f, pre ::: List(a, b) :_*)
                ret.copyFrom(tm)
                ret
              case IsSeq(pre, STeX.flatseq(ls), Nil) if ls.length > 2 =>
                getRules.get(classOf[ConjunctionRule]).toList match {
                  case ConjunctionRule(p) :: _ =>
                    val ret = ls.init.init.foldRight(SOMA(f, pre ::: List(ls.init.last, ls.last) :_*))((t, r) =>
                      SOMA(OMS(p), r, SOMA(f, pre ::: List(t, ls.last) :_*))
                    )
                    ret.copyFrom(tm)
                    ret
                  case _ =>
                    val ret = SOMA(f,reordered :_*)
                    ret.copyFrom(tm)
                    ret
                }
              case IsSeq(pre, STeX.flatseq(ls), List(b)) if ls.length > 2 =>
                getRules.get(classOf[ConjunctionRule]).toList match {
                  case ConjunctionRule(p) :: _ =>
                    val ret = ls.init.foldRight(SOMA(f, pre ::: List(ls.last, b) :_*))((t, r) =>
                      SOMA(OMS(p), r, SOMA(f, pre ::: List(t, b) :_*))
                    )
                    ret.copyFrom(tm)
                    ret
                  case _ =>
                    val ret = SOMA(f,reordered :_*)
                    ret.copyFrom(tm)
                    ret
                }
              case _ =>
                // TODO
                val ret = SOMA(f,reordered :_*)
                ret.copyFrom(tm)
                ret
            }
          case Some("bin" | "binr") =>
            reordered match {
              case IsSeq(Nil, OMV(v), rest) =>
                val fcont = getVariableContext ::: cont
                val x = Context.pickFresh(fcont, LocalName("foldrightx"))._1
                val y = Context.pickFresh(fcont, LocalName("foldrighty"))._1
                val tp = fcont.findLast(_.name == v) match {
                  case Some(v) if v.tp.isDefined =>
                    v.tp match {
                      case Some(STeX.flatseq.tp(t)) => t
                      case _ =>
                        val vn = getUnknownTp
                        names._1.unknowns = names._1.unknowns ::: vn :: Nil
                        makeUnknown(vn)
                    }
                  case _ =>
                    val vn = getUnknownTp
                    names._1.unknowns = names._1.unknowns ::: vn :: Nil
                    makeUnknown(vn)
                }
                val nf = traverse(f)
                val ret = if (rest.isEmpty) STeX.seqfoldright(STeX.seqlast(OMV(v)), STeX.seqinit(OMV(v)), x, tp, y, tp, SOMA(nf, OMV(x), OMV(y)))
                else STeX.seqfoldright(SOMA(nf, STeX.seqlast(OMV(v)) :: rest :_*), STeX.seqinit(OMV(v)), x, tp, y, tp, SOMA(nf, OMV(x), OMV(y)))
                ret.copyFrom(tm)
                ret
              case IsSeq(Nil, STeX.flatseq(tms), Nil) if tms.length >= 2 =>
                val ret = tms.init.init.foldRight(SOMA(f, tms.init.last, tms.last))((a, r) => SOMA(f, a, r))
                ret.copyFrom(tm)
                ret
              case IsSeq(Nil, STeX.flatseq(tms), rest) if tms.nonEmpty && (tms ::: rest).length >= 2 =>
                val ret = if (rest.isEmpty) tms.init.init.foldRight(SOMA(f, tms.last, tms.init.last))((a, r) => SOMA(f, a, r))
                else tms.init.foldRight(SOMA(f, tms.last :: rest :_*))((a, r) => SOMA(f, a, r))
                ret.copyFrom(tm)
                ret
              case _ =>
                val ret = SOMA(f, reordered :_*)
                ret.copyFrom(tm)
                ret
            }

          case Some("binl") =>
            reordered match {
              case IsSeq(Nil, OMV(v), rest) =>
                val fcont = getVariableContext ::: cont
                val x = Context.pickFresh(fcont, LocalName("foldleftx"))._1
                val y = Context.pickFresh(fcont, LocalName("foldlefty"))._1
                val tp = fcont.findLast(_.name == v) match {
                  case Some(v) if v.tp.isDefined =>
                    v.tp match {
                      case Some(STeX.flatseq.tp(t)) => t
                      case _ =>
                        val vn = getUnknownTp
                        names._1.unknowns = names._1.unknowns ::: vn :: Nil
                        makeUnknown(vn)
                    }
                  case _ =>
                    val vn = getUnknownTp
                    names._1.unknowns = names._1.unknowns ::: vn :: Nil
                    makeUnknown(vn)
                }
                val nf = traverse(f)
                val ret = if (rest.isEmpty) STeX.seqfoldleft(STeX.seqhead(OMV(v)), STeX.seqtail(OMV(v)), x, tp, y, tp, SOMA(nf, OMV(x), OMV(y)))
                else STeX.seqfoldleft(SOMA(nf, STeX.seqhead(OMV(v)) :: rest: _*), STeX.seqtail(OMV(v)), x, tp, y, tp, SOMA(nf, OMV(x), OMV(y)))
                ret.copyFrom(tm)
                ret
              case IsSeq(Nil, STeX.flatseq(tms), Nil) if tms.length >= 2 =>
                val ret = tms.tail.tail.foldLeft(SOMA(f, tms.tail.head, tms.head))((r, a) => SOMA(f, a, r))
                ret.copyFrom(tm)
                ret
              case IsSeq(Nil, STeX.flatseq(tms), rest) if tms.nonEmpty && (tms ::: rest).length >= 2 =>
                val ret = if (rest.isEmpty) tms.tail.tail.foldLeft(SOMA(f, tms.head, tms.tail.head))((r, a) => SOMA(f, a, r))
                else tms.tail.foldLeft(SOMA(f, tms.head :: rest: _*))((r, a) => SOMA(f, a, r))
                ret.copyFrom(tm)
                ret
              case IsSeq(List(a),STeX.flatseq(List(t)),Nil) =>
                val ret = SOMA(f,a,t)
                ret.copyFrom(tm)
                ret
              case _ =>
                val ret = SOMA(f, reordered: _*)
                ret.copyFrom(tm)
                ret
            }
          case _ =>
            val ret = SOMA(f,reordered :_*)
            ret.copyFrom(tm)
            ret
        }
      }

      def reorder(tm: OMBINDC): OMBINDC = {
        val (f, args) = SOMB.unapply(tm).getOrElse {
          return tm
        }
        val (ros, assoc) = getRuleInfo(f).getOrElse {
          return tm
        }
        val reordered = ros match {
          case Nil => args
          case ls => ls.map(args(_))
        }
        assoc match {
          case None => SOMB(f, reordered: _*)
          case Some("pre") =>
            reordered match {
              case SCtx(ctx) :: rest if ctx.nonEmpty =>
                val ret = ctx.variables.init.foldRight(SOMB(f, SCtx(Context(ctx.variables.last)) :: rest: _*))((vd, t) =>
                  SOMB(f, SCtx(Context(vd)), t)
                )
                ret.copyFrom(tm)
                ret
              case _ => tm
            }
        }
      }

      def getArgs(tp: Term): List[LocalName] = tp match {
        case STeX.implicit_binder(_, _, bd) => getUnknown :: getArgs(bd)
        case _ => Nil
      }

      def getTerm(n: LocalName): Option[Term] = getVariableContext.findLast(_.name == n).flatMap { vd =>
        vd.tp match {
          case Some(t) => Some(t)
          case None => vd.df match {
            case Some(t) => Some(t)
            case _ => None
          }
        }
      }

      def getTerm(p: GlobalName): Option[Term] = controller.getO(p).flatMap {
        case c: Constant =>
          c.tp match {
            case Some(t) => Some(t)
            case _ => c.df match {
              case Some(t) => Some(t)
              case _ => None
            }
          }
        case _ => None
      }

      def getOriginal(tm : Term, fieldname : LocalName) : Option[Constant] =
        OMDocHTML.getOriginal(tm,fieldname)(controller,getVariableContext)

      def recurse(args: List[SOMBArg])(implicit con: Context, names: (NameSet,Boolean)): List[SOMBArg] = {
        var icon = Context.empty
        args.map {
          case STerm(tm) => STerm(traverse(tm)(con ++ icon, names))
          case SCtx(ctx) =>
            val ret = traverseContext(ctx)(con ++ icon, names)
            ret.copyFrom(ctx)
            icon = icon ++ ret
            SCtx(ret)
        }
      }

      def makeUnknown(ln: LocalName)(implicit con: Context, names: (NameSet,Boolean)) = Solver.makeUnknown(ln,con.map(v => OMV(v.name)).distinct)//OMAorAny(OMV(ln), con.map(v => OMV(v.name)).distinct)

      override def traverse(t: Term)(implicit con: Context, names: (NameSet,Boolean)): Term = t match {
        case OMV(n) =>
          getVariableContext.findLast(_.name == n).foreach { vd =>
            vd.tp.foreach(traverse)
            vd.df.foreach(traverse)
          } // required to get free / implicit arguments
          if (names._2) {
            getTerm(n) match {
              case Some(tm) => getArgs(tm) match {
                case Nil => t
                case ls =>
                  names._1.unknowns = names._1.unknowns ::: ls
                  SOMA(t, ls.map(makeUnknown) :_*)
              }
              case _ => t
            }
          } else t
        case OMS(p) =>
          val nt = controller.getO(p) match {
            case Some(d : Declaration) =>
              val r = OMS(d.path)
              r.copyFrom(t)
              r
            case _ => t
          }
          if (names._2) getTerm(p) match {
            case Some(tm) => getArgs(tm) match {
              case Nil => nt
              case ls =>
                names._1.unknowns = names._1.unknowns ::: ls
                SOMA(nt,ls.map(makeUnknown) :_*)
            }
            case None => nt
          } else nt
        case o@SOMA(_,_) =>
          val t = reorder(o.asInstanceOf[OMA])
          t match {
            case SOMA(f, args) =>
              val ret = (f match {
                case OMV(n) => getTerm(n)
                case OMS(p) => getTerm(p)
                case Getfield(t, f) => getOriginal(t, f).flatMap(c => getTerm(c.path))
                case _ => None
              }) match {
                case Some(tm) =>
                  val ls = getArgs(tm)
                  names._1.unknowns = names._1.unknowns ::: ls
                  SOMA(traverse(f)(con, (names._1, false)), ls.map(makeUnknown) ::: args.map(traverse) :_*)
                case None => SOMA(traverse(f)(con, (names._1, false)), args.map(traverse) :_*)
              }
              ret.copyFrom(o)
              ret
            case SOMB(f, args) =>
              (f match {
                case OMV(n) => getTerm(n)
                case OMS(p) => getTerm(p)
                case Getfield(t,f) => getOriginal(t,f).flatMap(c => getTerm(c.path))
                case _ => None
              }) match {
                case Some(tm) =>
                  val ls = getArgs(tm)
                  names._1.unknowns = names._1.unknowns ::: ls
                  SOMB(traverse(f)(con,(names._1,false)), ls.map(n => STerm(makeUnknown(n))) ::: recurse(args): _*)
                case None => SOMB(traverse(f)(con,(names._1,false)), recurse(args): _*)
              }
          }
        case b@OMBINDC(_, _, _) =>
          val ret = reorder(b) match {
            case SOMB(f, args) =>
              (f match {
                case OMV(n) => getTerm(n)
                case OMS(p) => getTerm(p)
                case Getfield(t,f) => getOriginal(t,f).flatMap(c => getTerm(c.path))
                case _ => None
              }) match {
                case Some(tm) =>
                  val ls = getArgs(tm)
                  names._1.unknowns = names._1.unknowns ::: ls
                  SOMB(traverse(f)(con,(names._1,false)), ls.map(n => STerm(makeUnknown(n))) ::: recurse(args): _*)
                case None => SOMB(traverse(f)(con,(names._1,false)), recurse(args): _*)
              }
            case o =>
              //println("urgh")
              Traverser(this,o)
          }
          ret.copyFrom(b)
          ret
        case _ => Traverser(this, t)
      }
    }
    val names = new NameSet
    val freeVars = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMV(n) if !con.isDeclared(n) && t.metadata.get(ParseResult.unknown).isEmpty && names.frees.forall(_.name != n) =>
          val d = getVariableContext.findLast(_.name == n) match {
            case Some(vd) if vd.tp.isDefined =>
              vd.tp.foreach(traverse)
              vd.df.foreach(traverse)
              vd
            case Some(vd) =>
              vd.df.foreach(traverse)
              val ntp = getUnknownTp
              names.unknowns = names.unknowns ::: ntp :: Nil
              val tp = Solver.makeUnknown(ntp,(names.frees.reverse.map(i => OMV(i.name))/* ::: con.map(v => OMV(v.name))*/).distinct)
              val v = VarDecl(n,None,Some(tp),vd.df,None)
              v.copyFrom(vd)
              v
            case _ =>
              val ntp = getUnknownTp
              names.unknowns = names.unknowns ::: ntp :: Nil
              val tp = Solver.makeUnknown(ntp,(names.frees.reverse.map(i => OMV(i.name))/* ::: con.map(v => OMV(v.name))*/).distinct)
              VarDecl(n,tp)
          }
          names.frees ::= d
          t
        case OMV(n) if !con.isDeclared(n) && t.metadata.get(ParseResult.unknown).nonEmpty =>
          names.unknowns = names.unknowns ::: n :: Nil
          t
        case OMA(f, args) =>
          traverse(f)
          args.foreach(traverse)
          t
        case _ => Traverser(this,t)
      }
    }
    val hoas = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: Unit): Term = t match {
        case SOMA(head,args) =>
          OMDocHTML.getRuler(head)(controller,context) match {
            case None => Traverser(this,t)
            case Some(md) => OMDocHTML.getHOAS(md) match {
              case None =>
                head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        val r = OMA(OMS(o),args.map(a => apply(a, ())))
                        r.copyFrom(t)
                        r
                    }.getOrElse(Traverser(this,t))
                  case _ =>
                    Traverser(this,t)
                }
              case Some(hoas) =>
                val r = head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        hoas(OMS(o),args.map(a => apply(a,())))
                    }.getOrElse(hoas(head,args.map(a => apply(a,()))))
                  case _ =>
                    hoas(head,args.map(a => apply(a,())))
                }
                r.copyFrom(t)
                r
            }
          }
        case OMA(head, args) =>
          OMDocHTML.getRuler(head)(controller, context) match {
            case None => Traverser(this,t)
            case Some(md) => OMDocHTML.getHOAS(md) match {
              case None =>
                head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        val r = OMA(OMS(o), args.map(a => apply(a, ())))
                        r.copyFrom(t)
                        r
                    }.getOrElse(Traverser(this,t))
                  case _ =>
                    Traverser(this,t)
                }
              case Some(hoas) =>
                val r = head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        hoas(OMS(o), args.map(a => apply(a, ())))
                    }.getOrElse(hoas(head, args.map(a => apply(a, ()))))
                  case _ =>
                    hoas(head, args.map(a => apply(a, ())))
                }
                r.copyFrom(t)
                r
            }
          }
        case SOMB(head,List(SCtx(Context(vd)),STerm(bd))) =>
          OMDocHTML.getRuler(head)(controller, context) match {
            case None => Traverser(this,t)
            case Some(md) => OMDocHTML.getHOAS(md) match {
              case None =>
                head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        val r = OMBIND(OMS(o), Context(this.traverseObject(vd)),apply(bd,()))
                        r.copyFrom(t)
                        r
                    }.getOrElse(Traverser(this,t))
                  case _ =>
                    Traverser(this,t)
                }
              case Some(hoas) =>
                val r = head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        hoas(OMS(o), this.traverseObject(vd),apply(bd,()))
                    }.getOrElse(hoas(head, this.traverseObject(vd),apply(bd,())))
                  case _ =>
                    hoas(head, this.traverseObject(vd),apply(bd,()))
                }
                r.copyFrom(t)
                r
            }
          }
        case OMBIND(head,Context(vd),bd) =>
          OMDocHTML.getRuler(head)(controller, context) match {
            case None => Traverser(this,t)
            case Some(md) => OMDocHTML.getHOAS(md) match {
              case None =>
                head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        val r = OMBIND(OMS(o), Context(this.traverseObject(vd)), apply(bd, ()))
                        r.copyFrom(t)
                        r
                    }.getOrElse(Traverser(this,t))
                  case _ =>
                    Traverser(this,t)
                }
              case Some(hoas) =>
                val r = head match {
                  case OMS(s) =>
                    getRules.get(classOf[SubstRule]).collectFirst {
                      case SubstRule(`s`, o) =>
                        hoas(OMS(o), this.traverseObject(vd), apply(bd, ()))
                    }.getOrElse(hoas(head, this.traverseObject(vd), apply(bd, ())))
                  case _ =>
                    hoas(head, this.traverseObject(vd), apply(bd, ()))
                }
                r.copyFrom(t)
                r
            }
          }
        case SOMA(OMS(s), args) =>
          getRules.get(classOf[SubstRule]).find {
            case SubstRule(`s`, o) =>
              return traverse(OMA(OMS(o), args))
            case _ => false
          }
          Traverser(this, t)
        case SOMB(OMS(s), List(SCtx(ctx), STerm(bd))) =>
          getRules.get(classOf[SubstRule]).find {
            case SubstRule(`s`, o) =>
              return traverse(OMBIND(OMS(o), ctx,bd))
            case _ => false
          }
          Traverser(this, t)
        case OMS(s) =>
          getRules.get(classOf[SubstRule]).find {
            case SubstRule(`s`, o) =>
              return OMS(o)
            case _ => false
          }
          t
        case _ => Traverser(this,t)
      }
    }
    freeVars(ntm,())
    val next = names.frees.reverse.distinct.foldRight(ntm)((ln, t) => {
      STeX.implicit_binder(Context(ln), t)
    })
    val ret = hoas(traverser(next,(names,true)),())
    val fin = if (names.unknowns.nonEmpty) OMBIND(OMS(ParseResult.unknown), names.unknowns.distinct.map(VarDecl(_)), ret) else ret
    SourceRef.copy(tm,fin)
    fin
  }

  def getHOAS:Option[STeXHOAS] = {
    var all = getRules.getAll.collect {
      case hn : HOASNotation => SimpleHOAS(hn.hoas)
      case nhn : NestedHOASNotation => NestedHOAS(nhn.obj,nhn.meta)
    }
    all.foreach {
      case NestedHOAS(_,m) =>
        all = all.filterNot{
          case SimpleHOAS(hn) if hn == m => true
          case NestedHOAS(obj,_) if obj == m => true
          case _ => false
        }
      case _ =>
    }
    all.headOption
  }

  private def currentParent = {
    _parent match {
      case Some(p : HasRuleContext) => Some(p)
      case Some(p) =>
        p.collectAncestor{case p : HasRuleContext => p}
    }
  }
  def context = currentParent.map(_.context).getOrElse(Context.empty) // currentParent.map(_.rules).getOrElse(Nil)
  def getVariableContext = (_parent match {
    case Some(p : HTMLGroupLike) => Some(p)
    case Some(p) => p.collectAncestor{case p : HTMLGroupLike => p}
  }).map(_.getVariables).getOrElse(Context.empty)

  private def simpleState = new HTMLParser.ParsingState(controller,rules)

  def makeBinder(tm : Term,assoc:Boolean) : Context = {
    (tm,assoc) match {
      case (STeX.flatseq(ls),true) => ls.flatMap(makeBinder(_,true))
      case _ =>
        val ntm = applyTerm(tm)
        getRules.get(classOf[BindingRule]).collectFirst{rl => rl(ntm,assoc)(this) match {
          case Some(ct) =>
            return ct
        }}
        val ret = Context(VarDecl(LocalName.empty,tp=tm))
        SourceRef.copy(tm,ret)
        ret
    }
  }


  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  private lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def check(se : StructuralElement) = try {
    eh match {
      case STeXLSPErrorHandler(_,cont) =>
        cont(0.5,"Checking " + se.path.toString)
      case _ =>
    }
    checker(se)(ce)
  } catch {
    case g:GetError =>
      if (!missings.contains(g.path)) {
        eh(g)
        g.path match {
          case mp : MPath => this.missings ::= mp
          case _ =>
        }
      }
    case e : info.kwarc.mmt.api.Error =>
      eh(e)
    case e : Throwable =>
      eh.apply(new api.Error(e.getMessage){}.setCausedBy(e))
  }

   */
}

class SearchOnlyState(server:STeXServer, rules : List[HTMLRule],eh : ErrorHandler, dpath : DPath) extends SemanticState(server,rules,eh,dpath) {
  /*override def add(se : StructuralElement) = {}
  override def endAdd[T <: StructuralElement](ce: ContainerElement[T]) = {}
  override def update(se: StructuralElement): Unit = {}
  override def applyTerm(tm : Term) = tm
  override def applyTopLevelTerm(tm: Term): Term = tm
  override def context = Context.empty
  override def getRules = new MutableRuleSet
  override def check(se: StructuralElement): Unit = {}

   */
}