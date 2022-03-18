package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.{DPath, ErrorHandler, GetError, LocalName, MMTTask, MPath, MutableRuleSet, Path, RuleSet, StructuralElement, utils}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Context, OMBIND, OMBINDC, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.symbols.RuleConstantInterpreter
import info.kwarc.mmt.stex.rules.{BindingRule, HTMLTermRule}
import info.kwarc.mmt.stex.{STeX, STeXError}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText}

import scala.collection.mutable

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
    val ntm = applyTerm(tm)
    val freeVars = new Traverser[(mutable.Set[LocalName],mutable.Set[LocalName])] {
      override def traverse(t: Term)(implicit con: Context, names: (mutable.Set[LocalName],mutable.Set[LocalName])): Term = t match {
        case OMV(n) if t.metadata.get(ParseResult.unknown).isEmpty =>
          names._1.addOne(n)
          t
        case OMV(n) =>
          names._2.addOne(n)
          t
        case STeX.implicit_binder(ln,_,bd) =>
          names._2.addOne(ln)
          bd
        case OMBINDC(_,ctx,_) =>
          val ret = Traverser(this,t)
          ctx.variables.foreach(v => names._1 -= v.name)
          ret
        case _ => Traverser(this,t)
      }
    }
    val frees : mutable.Set[LocalName] = mutable.Set()
    val unks : mutable.Set[LocalName] = mutable.Set()
    freeVars(ntm,(frees,unks))
    val next = frees.foldLeft(ntm)((t,ln) => {
      val tp = getVariableContext.find(_.name == ln) match {
        case Some(vd) if vd.tp.isDefined => vd.tp
        case _ => Some(markAsUnknown(OMV({
          val n = getUnknownTp
          unks.addOne(n)
          n
        })))
      }
      STeX.implicit_binder(ln,tp,t)
    })
    if (unks.nonEmpty) OMBIND(OMS(ParseResult.unknown), unks.toList.map(VarDecl(_)), next) else next
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
      val path = Path.parseM(g.shortMsg.drop("no backend applicable to ".length)) //FR: I've reworded many error messages, so this might not work anymore
      if (!missings.contains(path)) {
        eh(g)
        this.missings ::= path
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
      val path = Path.parseM(g.shortMsg.drop("no backend applicable to ".length)) // FR: I've reworded error messages, so this may not work anymore
      if (!missings.contains(path)) {
        eh(g)
        this.missings ::= path
      }
  }
}