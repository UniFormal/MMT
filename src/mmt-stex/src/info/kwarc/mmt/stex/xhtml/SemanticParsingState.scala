package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.{DPath, ErrorHandler, LocalName, MMTTask, StructuralElement}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Link, Module}
import info.kwarc.mmt.api.objects.{Context, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.stex.{PreDocument, STeX}

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
    }
  )
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

  def makeBinder(tm : Term) : Context = applyTerm(tm) match {
    case STeX.informal(n) =>
      n match {
        case _ if n.label == "mi" =>
          n.children.filterNot(_.isEmpty) match {
            case List(t : XHTMLText) =>
              addTransform({case `tm` => OMV(t.text)})
              VarDecl(LocalName(t.text))
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

  lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def checkDefault[A <: StructuralElement](se : A) : A = {
    se match {
      case c : Constant =>
        controller add c
        (c.tp,c.df) match {
          case (None,None) => return se
          case (None,Some(OMS(_))) => return se
          case _ =>
        }
        checker.apply(c)(ce)
        se
      case _ =>
        controller add se
        checker.apply(se)(ce)
        se
    }
  }

  def applySE[A <: StructuralElement](se: A, pe : PreElement) = {
    pe.metadata.toList.foreach(p => se.metadata.add(MetaDatum(p._1, p._2)))
    pe match {
      case a : XHTMLAnnotation =>
        a.node.getAnnotations.collectFirst{
          case sr : SourceRefAnnotation =>
            SourceRef.update(se,SourceRefAnnotation.toSourceRef(controller,sr))
        }
      case _ =>
    }
    _setransforms.collectFirst { case r if r.isDefinedAt(se,this) => r(se,this) }.getOrElse(checkDefault(se)).asInstanceOf[A]
  }

  private var _context : Context = Context.empty
  def getContext = _context
  private def inContext[A](ctx : Context)(f : => A) = {
    val oldctx = _context
    _context = _context ++ ctx
    try { f } finally {
      _context = oldctx
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
  private def buildTheory(parent : Document,th : PreTheory) = th.getElement(this).foreach { tI =>
    val t = applySE(tI, th)
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
  }
  private def buildDeclaration(parent : Module, da : DeclarationAnnotation) = da.getElement(this).foreach{dI =>
    val d = applySE(dI,da)
    d match {
      case l : Link if l.isImplicit =>
        controller.library.endAdd(l)
      case _ =>
    }
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