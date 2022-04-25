package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.{DPath, ErrorHandler, GetError, GlobalName, LocalName, MMTTask, MPath, MutableRuleSet, Path, RuleSet, StructuralElement, utils}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMPMOD, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.symbols.{Constant, RuleConstantInterpreter}
import info.kwarc.mmt.stex.rules.{BindingRule, Getfield, HTMLTermRule, ModelsOf, RecType}
import info.kwarc.mmt.stex.{SCtx, SOMB, SOMBArg, STeX, STeXError, STerm}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText}

import scala.collection.mutable
import scala.util.Try

class SemanticState(controller : Controller, rules : List[HTMLRule],eh : ErrorHandler, val dpath : DPath) extends HTMLParser.ParsingState(controller,rules) {
  override def error(s: String): Unit = eh(new STeXError(s,None,None))
  private var maindoc : HTMLDocument = null
  def doc = maindoc.doc
  var missings : List[MPath] = Nil
  var in_term = false
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
    (LocalName("") / "i" / unknowns_counter.toString,LocalName("") / "I" / unknowns_counter.toString)
  }
  def markAsUnknown(v:OMV) : OMV = {
    v.metadata.update(ParseResult.unknown,OMS(ParseResult.unknown))
    v
  }

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

  override protected def onTop(n : HTMLNode): Option[HTMLNode] = {
    val nn = new HTMLDocument(dpath,n)
    maindoc = nn
    Some(nn)
  }
  /*
  private val _transforms: List[PartialFunction[Term, Term]] =  List(
    {
      case STeX.informal(n) if n.startsWith("<mi") =>
        val node = HTMLParser.apply(n.toString())(simpleState)
        val ln = node.children.head.asInstanceOf[HTMLText].text
        OMV(LocalName(ln))
    }
  )
  private def substitute(subs : List[(Term,Term)], tm : Term) = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case tmi if subs.exists(_._1 == tmi) => subs.find(_._1 == tmi).get._2
      case _ => Traverser(this,t)
    }
  }.apply(tm,())
  private var _setransforms: List[PartialFunction[StructuralElement, StructuralElement]] = Nil
  def addTransformSE(pf: PartialFunction[StructuralElement, StructuralElement]) = _setransforms ::= pf

   */

  private val self = this
  private val traverser = new StatelessTraverser {
    def trans = getRules.get(classOf[HTMLTermRule])
    private def applySimple(t:Term) = trans.foldLeft(t)((it, f) => f.apply(it)(self).getOrElse(it))
    override def traverse(t: Term)(implicit con: Context, state: State): Term = {
      val ret = Traverser(this,t)
      val ret2 = applySimple(ret)
      ret2
    }
  }

  def applyTerm(tm: Term): Term = traverser(tm, ())
  def applyTopLevelTerm(tm : Term) = {
    val ntm = /* if (reorder.isEmpty) */ applyTerm(tm)
    /* else applyTerm(tm) match {
      case OMA(f,args) => OMA(f,reorder.map(args(_)))
      case SOMB(f, args) => SOMB(f,reorder.map(args(_)):_*)
      case t => t
    } */
    class NameSet {
      var frees: List[(LocalName,Option[Term])] = Nil
      var unknowns: List[LocalName] = Nil
    }
    val traverser = new Traverser[(NameSet,Boolean)] {
      def reorder(tm: OMA): OMA = {
        val head = tm match {
          case OMA(OMS(p),_) => p
          case OMA(OMV(_),_) => return tm
          case OMA(Getfield(t,f),_) => getOriginal(t,f).map(_.path).getOrElse{ return tm}
        }
        controller.getO(head) match {
          case Some(c: Constant) =>
            val reordered = OMDocHTML.getReorder(c) match {
              case Nil => tm.args
              case ls => ls.map(tm.args(_)) //OMA(tm.fun, ls.map(tm.args(_)))
            }
            tm.copy(args=reordered)
          case _ => tm
        }
      }

      def reorder(tm: OMBINDC): OMBINDC = tm match {
        case SOMB(f@OMS(p), args) =>
          controller.getO(p).collect {
            case c: Constant =>
              val reordered = OMDocHTML.getReorder(c) match {
                case Nil => args
                case ls => ls.map(args(_))
              }
              (OMDocHTML.getAssoctype(c),reordered) match {
                case (None,_) => return SOMB(f, reordered :_*)
                case (Some("pre"),SCtx(ctx) :: rest) if ctx.nonEmpty =>
                  val ret = ctx.variables.init.foldRight(SOMB(f,SCtx(Context(ctx.variables.last)) :: rest :_*))((vd,t) =>
                    SOMB(f,SCtx(Context(vd)),t)
                  )
                  ret.copyFrom(tm)
                  return ret
                case (Some(s),_) =>
                  println(s)
                  ???
              }
          }
          tm
        case _ => tm
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

      def getOriginal(tm : Term, fieldname : LocalName) : Option[Constant] = tm match {
        case OMV(n) => getVariableContext.findLast(_.name == n).flatMap { vd =>
          vd.tp match {
            case Some(ModelsOf(mod@OMPMOD(_,_))) =>
              Try(controller.library.get(mod,fieldname)).toOption match {
                case Some(c : Constant) => Some(c)
                case _ => None
              }
            case _ => None
          }
        }
        case OMS(p) => controller.getO(p).flatMap {
          case c : Constant =>
            c.tp match {
              case Some(ModelsOf(mod@OMPMOD(_,_))) =>
                Try(controller.library.get(mod,fieldname)).toOption match {
                  case Some(c : Constant) => Some(c)
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      }

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
                  OMA(t, ls.map(makeUnknown))
              }
              case _ => t
            }
          } else t
        case OMS(p) if names._2 =>
          getTerm(p) match {
            case Some(tm) => getArgs(tm) match {
              case Nil => t
              case ls =>
                names._1.unknowns = names._1.unknowns ::: ls
                OMA(t,ls.map(makeUnknown))
            }
            case None => t
          }
        case o: OMA =>
          val OMA(f, args) = reorder(o)
          val ret = (f match {
            case OMV(n) => getTerm(n)
            case OMS(p) => getTerm(p)
            case Getfield(t,f) => getOriginal(t,f).flatMap(c => getTerm(c.path))
            case _ => ???
          }) match {
            case Some(tm) =>
              val ls = getArgs(tm)
              names._1.unknowns = names._1.unknowns ::: ls
              OMA(traverse(f)(con,(names._1,false)), ls.map(makeUnknown) ::: args.map(traverse))
            case None => OMA(traverse(f)(con,(names._1,false)), args.map(traverse))
          }
          ret.copyFrom(o)
          ret
        case b@OMBINDC(_, _, _) =>
          val ret = reorder(b) match {
            case SOMB(f, args) =>
              (f match {
                case OMV(n) => getTerm(n)
                case OMS(p) => getTerm(p)
                case Getfield(t,f) => getOriginal(t,f).flatMap(c => getTerm(c.path))
                case _ => ???
              }) match {
                case Some(tm) =>
                  val ls = getArgs(tm)
                  names._1.unknowns = names._1.unknowns ::: ls
                  SOMB(traverse(f)(con,(names._1,false)), ls.map(n => STerm(makeUnknown(n))) ::: recurse(args): _*)
                case None => SOMB(traverse(f)(con,(names._1,false)), recurse(args): _*)
              }
            case o =>
              //println("urgh")
              o
          }
          ret.copyFrom(b)
          ret
        case _ => Traverser(this, t)
      }
    }
    val names = new NameSet
    val freeVars = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMV(n) if !con.isDeclared(n) && t.metadata.get(ParseResult.unknown).isEmpty && names.frees.forall(_._1 != n) =>
          val tp = getVariableContext.findLast(_.name == n) match {
            case Some(vd) if vd.tp.isDefined =>
              vd.tp.foreach(traverse)
              vd.tp
            case _ =>
              val n = getUnknownTp
              names.unknowns = names.unknowns ::: n :: Nil
              Some(Solver.makeUnknown(n,(names.frees.reverse.map(i => OMV(i._1)) ::: con.map(v => OMV(v.name))).distinct))
          }
          names.frees ::= (n,tp)
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
    freeVars(ntm,())
    val next = names.frees.reverse.distinct.foldRight(ntm)((ln, t) => {
      STeX.implicit_binder(ln._1, ln._2, t)
    })
    val ret = traverser(next,(names,true))
    if (names.unknowns.nonEmpty) OMBIND(OMS(ParseResult.unknown), names.unknowns.distinct.map(VarDecl(_)), ret) else ret
  }

  private def currentParent = {
    _parent match {
      case Some(p : HasRuleContext) => Some(p)
      case Some(p) =>
        p.collectAncestor{case p : HasRuleContext => p}
    }
  }
  def context = currentParent.map(_.context).getOrElse(Context.empty)
  def getRules = try {RuleSet.collectRules(controller,context)} catch {
    case g: GetError =>
      if (!missings.contains(g.path)) {
        eh(g)
        g.path match {
          case mp : MPath => this.missings ::= mp
          case _ =>
        }
      }
      new MutableRuleSet
  } // currentParent.map(_.rules).getOrElse(Nil)
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
          case Some(ct) => return ct
        }}
        Context(VarDecl(LocalName.empty,tp=tm))
    }
   /* val ntm = applyTerm(tm)
    //getRules.get(classOf[BindingRule]).collectFirst{case rl if rl(ntm)(this).isDefined => return rl(ntm)(this).get}
    ntm match {
      case STeX.informal(n) =>
       HTMLParser.apply(n.toString())(simpleState) match {
          case n if n.label == "mi" =>
            n.children.filterNot(_.isEmpty) match {
              case List(t : HTMLText) =>
                currentParent.foreach(_.addRule({case `tm` => OMV(t.text)}))
                val vd = VarDecl(LocalName(t.text))
                vd.metadata.update(STeX.meta_notation,tm)
                vd
              case _ =>
                print("")
                ???
            }
          case _ =>
            print("")
            ???
        }
      case _ =>
        print("")
        ???
    } */
  }


  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  private lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def check(se : StructuralElement) = try {
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
  }
}