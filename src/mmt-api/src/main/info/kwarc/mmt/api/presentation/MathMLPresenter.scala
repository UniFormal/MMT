package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import notations._
import symbols._
import objects._

import utils.xml._
import HTMLAttributes._

import scala.xml.{Attribute, Node, Null}

@deprecated("legacy alias for PresentationMathMLPresenter")
class MathMLPresenter extends PresentationMathMLPresenter {}

/**
 * This overrides the appropriate methods to present objects as Presentation MathML.
 *
 * The MathML carries additional attributes as described in [[HTMLAttributes]]
 */
class PresentationMathMLPresenter extends NotationBasedPresenter {

   /** generalized apply method that takes a callback function to determine the css class of a subterm */
   def apply(o: Obj, origin: Option[CPath], style: PresentationContext => String)(implicit rh : RenderingHandler) {
      val pc = preparePresentation(o, origin)
      implicit val pcS = pc.copy(style = Some(style))
      doToplevel(o) {
         recurse(o)
      }
   }

   protected def mathmlattribs(implicit pc: PresentationContext): List[(String, String)] = {
      var ret = List(position -> pc.pos.toString)

     pc.source.foreach {r =>
         ret ::= source -> r.toString
      }
      pc.style.foreach {s =>
         ret ::= "class" -> s(pc)
      }
      ret
   }
   override def doIdentifier(p: ContentPath)(implicit pc : PresentationContext) {
      val primaryName = controller.getO(p) match {
         case Some(d: Declaration) =>
           val (pn,_) = d.primaryNameAndAliases
           pn
         case _ => p.name
      }
      pc.html.mo(attributes = (symref -> p.toPath) :: mathmlattribs) {
        val parts = primaryName.simplify.map {
          case ComplexStep(t) => t.name.toString
          case SimpleStep(s) => s
        }
        pc.html.text(parts.mkString("/"))
      }
   }
   override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List(varref -> vd.declpos.toString)
      }
      pc.html.mi(attributes = vdAtt ::: mathmlattribs) {
        super.doVariable(n)
      }
   }
   override def doLiteral(l: OMLITTrait)(implicit pc: PresentationContext) {
      val symAtt = l.synType match {
        case OMID(p) => List(symref -> p.toPath)
        case _ => Nil
      }
      pc.html.mn(attributes = symAtt ::: mathmlattribs) {
        super.doLiteral(l)
      }
   }
   override def doOperator(s: String)(implicit pc : PresentationContext) {
      pc.html.mo {
        super.doOperator(s)
      }
   }
   override def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc : PresentationContext) {
      val mo = d.text match {
         case " " => element("mspace", ("width" -> ".2em") :: (symref -> p.toPath) :: mathmlattribs, "")
         case t   => element("mo", (symref -> p.toPath) :: mathmlattribs, t)
      }
      if (! implicits.isEmpty) {
         pc.html.mrow {
            pc.out(mo)
            implicits.foreach {i =>
               doImplicit {
                 doSpace(1)
                 i()
               }
            }
         }
      } else
         pc.out(mo)
   }
   override def doSpace(level: Int)(implicit pc : PresentationContext) {
      val ms = element("mspace", List(("width", "." + (2*level).toString + "em")), "")
      pc.out(ms)
   }
   override def doToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) {
      val nsAtts = List("xmlns" -> namespace("mathml"))
      val mmtAtts = pc.owner match {
         case None => Nil
         case Some(cp) => List(owner -> cp.parent.toPath, component -> cp.component.toString, position -> "")
      }
      // <mstyle displaystyle="true">
      pc.out(openTag("math", nsAtts ::: mmtAtts))
      body
      pc.out(closeTag("math"))
   }
   private def bracket(optional: Boolean, open: Boolean) = {
      val cls = if (optional) List(bracketsOpt, bracketsOptHidden) else List(brackets)
      val brack = if (open) "(" else ")"
      element("mo", List(("class", (operator::cls).mkString(" "))), brack)
   }
   /**
    * wraps brackets around argument
    * @param brackets None/Some(true)/Some(false) for no/hidden/shown brackets
    */
   protected def wrapBrackets(brackets: Option[Boolean], body: => Unit)(implicit pc: PresentationContext) {
      pc.out(openTag("mrow", mathmlattribs))
      brackets foreach {b => pc.out(bracket(b, true))}
      body
      brackets foreach {b => pc.out(bracket(b, false))}
      pc.out(closeTag("mrow"))
   }
   override def doBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(Some(false), body)
   }
   override def doUnbracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(None, body)
   }
   override def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(Some(true), body)
   }
   private def doOptional(group: String, body: => Unit)(implicit pc: PresentationContext) {
      pc.out(openTag("mrow", List(("class", s"$group ${hidden(group)}"))))
      body
      pc.out(closeTag("mrow"))
   }
   override def doImplicit(body: => Unit)(implicit pc: PresentationContext) {
      doOptional(implicitarg, body)
   }
   override def doInferredType(body: => Unit)(implicit pc: PresentationContext) {
      doOptional(reconstructedtype, body)
   }

   /** wraps continuations in mrow to make sure they produce a single element */
   private def R(c: Cont)(implicit pc: PresentationContext) = pc.html.mrow {c()}
   override def doScript(main: => Unit, sup: Option[Cont], sub: Option[Cont], over: Option[Cont], under: Option[Cont])(implicit pc: PresentationContext) {
      import pc.html._
      def underover(mbp: Cont) {
         (under, over) match {
            case (Some(u), Some(o)) => munderover {mbp(); R(u); R(o)}
            case (Some(u), None)    => munder     {mbp(); R(u)}
            case (None, Some(o))    => mover      {mbp(); R(o)}
            case (None,None)        => mbp()
         }
      }
      def subsup() {
         (sub, sup) match {
            case (Some(b), Some(p)) => pc.html.msubsup {main; R(b); R(p)}
            case (Some(b), None)    => pc.html.msub    {main; R(b)}
            case (None, Some(p))    => pc.html.msup    {main; R(p)}
            case (None,None)        => main
         }
      }
      underover {subsup _}
   }
   override def doFraction(above: List[Cont], below: List[Cont], line: Boolean)(implicit pc: PresentationContext) {
      pc.html.mfrac {
         pc.html.mrow(attributes = List("linethickness" -> (if (line) "" else "0px"))) {
            above foreach {a => a()}
         }
         pc.html.mrow {
            below foreach {b => b()}
         }
      }
   }

   override def doTd(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.td {
       pc.html.mrow {
         ms foreach {a => a()}
       }
     }
   }

   override def doTr(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.tr {
       ms foreach {a => a()}
     }
   }

   override def doTable(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.table {
       ms foreach {a => a()}
     }
   }
   override def doSqrt(args : List[Cont])(implicit pc: PresentationContext) {
     pc.html.msqrt {
       pc.html.mrow {
         args foreach{a => a()}
       }
     }
   }

   override def doRootMarker(base : List[Cont], root : List[Cont])(implicit pc: PresentationContext) {
     if(root != Nil){
       pc.html.mroot{
         pc.html.mrow {
         base foreach(a => a())
         }
         doSqrt(base)
       }
     }
     else doSqrt(base)
   }

   override def doNumberMarker(arg : Delim)(implicit pc: PresentationContext) {
     pc.html.mn {
       pc.out(arg.s)
     }
   }

   override def doIdenMarker(arg : Delim)(implicit pc: PresentationContext) {
     pc.html.mi {
       pc.out(arg.s)
     }
   }

   override def doErrorMarker(args: List[Cont])(implicit pc: PresentationContext){
     pc.html.merror{
     args foreach{m => m()}
     }
   }

   override def doPhantomMarker(args: List[Cont])(implicit pc: PresentationContext){
     pc.html.mphantom{
     args foreach {a => a()}
     }
   }
   override def doGlyphMarker(arg : Delim, alt : String = "Failed to load")(implicit pc:PresentationContext){
      pc.html.mglyph(attributes = List("src" -> arg.s, "alt"->alt) ) {}
   }

   override def doLabelMarker(args : List[Cont], label : String) (implicit pc:PresentationContext) {
     pc.html.mlabel{
       pc.html.mtext{
         pc.out(label)
       }
       pc.html.mrow{
         args foreach{m => m()} // have to be TdMarkers
       }
     }
   }

   override def doTextMarker(text : Delim)(implicit pc: PresentationContext){
     pc.html.mtext{
       pc.out(text.s)
     }
   }
}


/** ContentMathMLPresenter presents objects as ContentMathML */
class ContentMathMLPresenter extends ObjectPresenter {
  def apply(o: Obj, origin: Option[CPath])(implicit rh: RenderingHandler): Unit = {
    val node = ContentMathMLPresenter.applyContext(o)(MathMLContext.forContent(Nil, origin))
    rh(node)
  }
}

object ContentMathMLPresenter {
  def apply(o: Obj): scala.xml.Node = applyContext(o)(MathMLContext.forContent(Nil, None))
  def apply(sf: SemiFormalObject): scala.xml.Node = applyContext(sf)(MathMLContext.forContent(Nil, None))

  def applyContext(o: Obj)(implicit context: MathMLContext): scala.xml.Node = context(applyImpl(o))
  def applyContext(sf: SemiFormalObject)(implicit context: MathMLContext): scala.xml.Node = context(applyImpl(sf))

  /** turn an object into a ContentMathML node */
  private def applyImpl(o: Obj)(implicit context: MathMLContext): scala.xml.Node = o match {
    case VarDecl(name, _, tp, df, _) => <bvar><ci>{name.toPath}</ci>{objChildren(tp.toList, df.toList)}</bvar>
    case Context(variables @ _*) => <apply>{objChildren(variables.toList)}</apply>
    case Sub(name, target) => <mi name={name.toPath}>{objChildren(List(target))}</mi>
    case s: Substitution => applyImpl(s.asContext)
    case OMID(path) => <csymbol>{path.toPath}</csymbol>
    case OMBINDC(binder, ctx, scopes) => <apply>{objChildren(List(binder), ctx.toList, scopes)}</apply>
    case OMA(fun, args) => <apply>{objChildren(List(fun), args)}</apply>
    case OMV(name) =>
      if (context.qvars.isDeclared(name)) <mws:qvar xmlns:mws="http://www.mathweb.org/mws/ns">{name.toPath}</mws:qvar>
      else <ci>{name.toPath}</ci>
    case OMATTR(arg, key, value) => <apply><csymbol>OMATTR</csymbol>{objChildren(List(arg, key, value))}</apply>
    case OMFOREIGN(node) => <apply><csymbol>OMFOREIGN</csymbol>{node}</apply>
    case OMSemiFormal(tokens) => <apply><csymbol>OMSemiFormal</csymbol>{sfChildren(tokens)}</apply>
    case l: OML => <label>{objChildren(List(l.vd))}</label>
    case t: OMLITTrait => addAttrOrChild(<cn encoding="mmt-literal">{t.toString}</cn>, "definitionURL", t.synTypeXML)

    case _ => unexpectedError("Unsupported subobject")
  }

  /** turn a semiformal object into a MathMLNode */
  private def applyImpl(sf: SemiFormalObject)(implicit context: MathMLContext): scala.xml.Node = sf match {
    case Text(format, obj) => <mtext format={format}>{scala.xml.PCData(obj)}</mtext>
    case XMLNode(obj) => obj
    case Formal(obj) => applyImpl(obj)

    case _ => unexpectedError("Unsupported SemiFormal Subobject")
  }

  private def unexpectedError(message: String): scala.xml.Node = {
    <cerror><csymbol cd="moreerrors">unexpected</csymbol><cs>{message}</cs></cerror>
  }

  /** recursion helper that maps the provided object children to nodex */
  private def objChildren(children: Seq[Obj]*)(implicit context: MathMLContext): List[Node] = {
    children.flatten.zipWithIndex.map({ case (o, i) => applyContext(o)(context / i)}).toList
  }

  /** recursion helper that maps the provided semiformal to nodex */
  private final def sfChildren(children: Seq[SemiFormalObject]*)(implicit context: MathMLContext): List[Node] = {
    children.flatten.zipWithIndex.map({ case (o, i) => applyContext(o)(context / i)}).toList
  }

}

/** presents objects as Parallel Markup (Content + Presentation) MathML */
class ParallelMathMLPresenter extends PresentationMathMLPresenter {
  override def doToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) {
    val nsAtts = List("xmlns" -> namespace("mathml"), "xmlns:jobad" -> namespace("jobad"))
    val mmtAtts = pc.owner match {
      case None => Nil
      case Some(cp) => List("jobad:owner" -> cp.parent.toPath, "jobad:component" -> cp.component.toString, "jobad:mmtref" -> "")
    }
    val idAtt = ( "id" -> o.hashCode.toString)
    // <mstyle displaystyle="true">
    pc.out(openTag("math",  idAtt :: nsAtts ::: mmtAtts))
    pc.out(openTag("semantics", Nil))
    body
    pc.out(openTag("annotation-xml", List("encoding" -> "MathML-Content")))
    pc.out(ContentMathMLPresenter.applyContext(o)(MathMLContext.forPresentation(pc)).toString)
    pc.out(closeTag("annotation-xml"))
    pc.out(closeTag("semantics"))
    pc.out(closeTag("math"))
  }

  override def mathmlattribs(implicit pc: PresentationContext): List[(String, String)] = {
    var ret = super.mathmlattribs

    // add id and xref attributes, if we have an origin
    pc.owner.foreach(o => {
      ret ::= "id" -> MathMLContext.presentationID(o, pc.pos)
      ret ::= "xref" -> MathMLContext.contentID(o, pc.pos)
    })

    ret
  }
}

/**
  * A MathML Context is a context used for exporting objects as MathML
  *
  * @param qvars qvars
  * @param position The current position within the term being presentated
  * @param origin Optional origin for the object being exported. When true, will set an xref attribute pointing to an appropriate Presentation object.
  *
  */
case class MathMLContext(qvars: Context, position: Position, origin: Option[CPath], isPresentation: Boolean) {
  def /(i: Int, newContext: Context): MathMLContext = MathMLContext(newContext, position / i, origin, isPresentation)
  def /(i: Int): MathMLContext = MathMLContext(qvars, position / i, origin, isPresentation)

  /** apply sets the id and xref attributes on a node, if needed */
  def apply(node: Node): Node = node match {
    case e: scala.xml.Elem => attrs.foldLeft(e)((ee, kv) => ee % Attribute("", kv._1, kv._2, Null))
    case _ => node
  }

  def attrs: List[(String, String)] = origin.map(o => {
    if(isPresentation) {
      ("id", MathMLContext.presentationID(o, position)) :: ("xref", MathMLContext.contentID(o, position)) :: Nil
    } else {
      ("id", MathMLContext.contentID(o, position)) :: ("xref", MathMLContext.presentationID(o, position)) :: Nil
    }
  }).getOrElse(Nil)

}

object MathMLContext {
  def forContent(qvars: Context, origin: Option[CPath]): MathMLContext = MathMLContext(qvars, Position(Nil), origin, isPresentation = true)
  def forPresentation(context: PresentationContext): MathMLContext = MathMLContext(Context(), context.pos, context.owner, isPresentation = false)

  private def ID(prefix: String, origin: CPath, position: Position): String = {
    prefix + "__" + position.toString + "__" + origin.toPath
  }
  /** ID of a presentation mathml element */
  def presentationID(origin: CPath, position: Position): String = ID("mmt_cml", origin, position)
  /** ID of a content mathml element */
  def contentID(origin: CPath, position: Position): String = ID("mmt_pml", origin, position)
}
