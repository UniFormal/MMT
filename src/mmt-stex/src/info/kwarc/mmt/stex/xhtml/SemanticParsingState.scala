package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.{DPath, ErrorHandler, GetError, LocalName, MMTTask, MPath, Path, RuleSet, StructuralElement}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Link, Module, ModuleOrLink, Theory}
import info.kwarc.mmt.api.objects.{Context, OMA, OMBINDC, OMID, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, RuleConstantInterpreter}
import info.kwarc.mmt.stex.features.BindingRule
import info.kwarc.mmt.stex.{STeX, STeXError}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText}

import scala.collection.mutable

class SemanticState(controller : Controller, rules : List[HTMLRule],eh : ErrorHandler, val dpath : DPath) extends HTMLParser.ParsingState(controller,rules) {
  override def error(s: String): Unit = eh(new STeXError(s,None,None))
  private var maindoc : HTMLDocument = null
  def doc = maindoc.doc
  var missings : List[MPath] = Nil

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

  private val traverser = new StatelessTraverser {
    def trans = currentParent.map(_.rules).getOrElse(Nil).reverse ::: _transforms
    override def traverse(t: Term)(implicit con: Context, state: State): Term = {
      val ret = trans.foldLeft(t)((it, f) => f.unapply(it).getOrElse(it))
      Traverser(this, ret)
    }
  }

  def applyTerm(tm: Term): Term = traverser(tm, ())
  def applyTopLevelTerm(tm : Term) = {
    val ntm = applyTerm(tm)
    ntm.freeVars.foldLeft(ntm)((t,ln) => STeX.universal_quantifier(ln,None,t)) // TODO infer types or something?
  }

  private def currentParent = _parent.flatMap(_.collectAncestor{case p : OMDocParent => p})

  def context = currentParent.map(_.context).getOrElse(Context.empty)
  def getRules = RuleSet.collectRules(controller,context) // currentParent.map(_.rules).getOrElse(Nil)

  private def simpleState = new HTMLParser.ParsingState(controller,rules)

  def makeBinder(tm : Term) : Context = {
    val ntm = applyTerm(tm)
    getRules.get(classOf[BindingRule]).collectFirst{case rl if rl(ntm)(this).isDefined => return rl(ntm)(this).get}
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
    }
  }

  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  private lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def check(se : StructuralElement) = try {
    checker(se)(ce)
  } catch {
    case g@GetError(s) =>
      eh(g)
      val path = Path.parseM(s.drop("no backend applicable to ".length))
      this.missings ::= path
  }
}



// ---------------------------------------------------------------------------------------------------------------------

/*
class SemanticParsingState(rules : List[PartialFunction[(XHTMLNode,XHTMLParsingState), Unit]],eh : ErrorHandler, val dpath : DPath, controller : Controller)
  extends XHTMLParsingState(rules,eh) {
  val maindoc = new PreDocument(dpath)
  private var _transforms: List[PartialFunction[Term, Term]] = List(
    {
      case STeX.informal(n) if n.label == "mi" && {
        n.children.filterNot(_.isEmpty) match {
          case List(_: XHTMLText) => true
          case _ => false
        }
      } =>
        val ln = n.children.filterNot(_.isEmpty).head.asInstanceOf[XHTMLText].text
        OMV(LocalName(ln))
    },
    {
      case tm@OMA(OMID(sym),STeX.flatseq(bnds) :: rest) if tm.metadata.getValues(STeX.meta_quantification).contains(OMS(STeX.meta_quantification)) =>
          val nm = tm.metadata.getAll.filterNot(m => m.key == STeX.meta_quantification && m.value == OMS(STeX.meta_quantification))
          val ctx = bnds.foldLeft(Context.empty)((c,t) => c ++ makeBinder(t))
          val subs = ctx.map(vd => (vd.metadata.getValues(STeX.meta_notation).head.asInstanceOf[Term],OMV(vd.name)))
          val ntm = OMBINDC(OMID(sym),ctx,rest.map(r => substitute(subs,r)))
          ntm.metadata.add(nm :_*)
          ntm
        // tm.metadata.update(STeX.meta_quantification,OMS(STeX.meta_quantification))
    }
  )
  private def substitute(subs : List[(Term,Term)], tm : Term) = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case tmi if subs.exists(_._1 == tmi) => subs.find(_._1 == tmi).get._2
      case _ => Traverser(this,t)
    }
  }.apply(tm,())
  private var _setransforms: List[PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]] = Nil

  def addTransform(pf: PartialFunction[Term, Term]) = _transforms ::= pf
  def addTransformSE(pf: PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]) = _setransforms ::= pf

  private val traverser = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = {
      val ret = _transforms.foldLeft(t)((it, f) => f.unapply(it).getOrElse(it))
      Traverser(this, ret)
    }
  }

  def applyTerm(tm: Term): Term = traverser(tm, ())

  def applyTopLevelTerm(tm : Term) = {
    val ntm = applyTerm(tm)
    ntm.freeVars.foldLeft(ntm)((t,ln) => STeX.universal_quantifier(ln,None,t)) // TODO infer types or something?
  }

  def markBinder(tm : Term) : Unit = {
    tm.metadata.update(STeX.meta_quantification,OMS(STeX.meta_quantification))
  }

  private def getRules = RuleSet.collectRules(controller,_context)

  def makeBinder(tm : Term) : Context = {
    val ntm = applyTerm(tm)
    getRules.get(classOf[BindingRule]).collectFirst{case rl if rl(ntm)(this).isDefined => return rl(ntm)(this).get}
    ntm match {
      case STeX.informal(n) =>
        n match {
          case _ if n.label == "mi" =>
            n.children.filterNot(_.isEmpty) match {
              case List(t : XHTMLText) =>
                addTransform({case `tm` => OMV(t.text)})
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
    }
  }

  lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def checkDefault[A <: StructuralElement](se : A, parent : Option[StructuralElement]) : A = try {
    se match {
      case c : Constant if parent.exists(_.isInstanceOf[DerivedDeclaration]) =>
        controller add c
        se
      case c : Constant if parent.exists(_.isInstanceOf[Theory]) =>
        controller add c
        (c.tp,c.df) match {
          case (None,None) => return se
          case (None,Some(OMS(_))) => return se
          case _ =>
        }
        checker.apply(c)(ce)
        se
      case d : DerivedDeclaration =>
        controller.add(d)
        se
      case _ =>
        controller add se
        checker.apply(se)(ce)
        se
    }
  } catch {
    case e : Error =>
      eh(e)
      se
  }

  def applySE[A <: StructuralElement](se: A, pe : PreElement, parent : Option[StructuralElement]) = {
    pe.metadata.toList.foreach(p => se.metadata.add(MetaDatum(p._1, p._2)))
    pe match {
      case a : XHTMLAnnotation =>
        a.node.getAnnotations.collectFirst{
          case sr : SourceRefAnnotation =>
            SourceRef.update(se,SourceRefAnnotation.toSourceRef(controller,sr))
        }
      case _ =>
    }
    _setransforms.collectFirst { case r if r.isDefinedAt(se,this) => r(se,this) }.getOrElse(checkDefault(se,parent)).asInstanceOf[A]
  }

  private var _context : Context = Context.empty
  def getContext = _context
  private def inContext[A](ctx : Context)(f : => A) = {
    val oldctx = _context
    val oldtransforms = _transforms
    _context = _context ++ ctx
    try { f } finally {
      _context = oldctx
      _transforms = oldtransforms
    }
  }

  def build = {
    val doc = new Document(dpath)
    controller add doc
    maindoc.children.foreach {
      case t : TheoryAnnotation =>
        t.subelements.foreach{t =>
          buildTheory(doc,t)
        }
      case _ =>
    }
    doc
  }
  private def buildTheory(parent : Document,th : PreTheory) = try {th.getElement(this).foreach { tI =>
    val t = applySE(tI, th, None)
    controller.getO(th.path.parent) match {
      case Some(d: Document) =>
      case _ => controller.add(new Document(th.path.parent))
    }
    controller add MRef(parent.path, t.path)
    inContext(Context(t.path)) {
      th.children.foreach {
        case d: DeclarationAnnotation =>
          d.subelements.foreach(buildDeclaration(t, _))
        case _ =>
      }
    }
    val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})
    checker.applyElementEnd(t)(ce)
  } } catch {
    case e : Error =>
      eh(e)
  }
  private def buildDeclaration(parent : ModuleOrLink, da : DeclarationAnnotation) : Unit = try {
    da.getElement(this).foreach { dI =>
      val d = applySE(dI, da, Some(parent))
      d match {
        case l: Link if l.isImplicit =>
          controller.library.endAdd(l)
        case dd: DerivedDeclaration =>
          inContext(Context(dd.path.toMPath)) {
            da.asInstanceOf[DerivedAnnotation].children.foreach {
              case d: DeclarationAnnotation =>
                d.subelements.foreach(buildDeclaration(dd.module, _))
              case _ =>
            }
          }
          controller.library.endAdd(dd)
          checker.apply(dd)(ce)
        case _ =>
      }
    }
  } catch {
    case e : Error =>
      eh(e)
  }

  private var elems: List[PreElement] = List(maindoc)

  def getParent = elems.head

  override def open(node: XHTMLNode): Unit = {
    super.open(node)
    node.getAnnotations.collect {
      case e: PreElement =>
        e.open(this)
        elems ::= e
    }
  }

  override def close(node: XHTMLNode): Unit = {
    super.close(node)
    node.getAnnotations.reverse.collect {
      case e : PreElement =>
        elems.headOption match {
          case Some(`e`) =>
            elems = elems.tail
            e.close(this)
          case _ =>
            print("")
            ???
        }
    }
  }

}

 */